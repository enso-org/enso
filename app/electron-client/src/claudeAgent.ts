/**
 * @file Electron-main-side adapter that shells out to the user-installed `claude` CLI
 * executable to generate the body of a User Defined Component from a natural-language
 * prompt. Exposed to the renderer via IPC; the renderer never invokes the CLI directly.
 *
 * Authentication rides on whatever the `claude` CLI is already configured with (OAuth,
 * keychain, `ANTHROPIC_API_KEY`, or subscription token). The main process does not require
 * or read the API key beyond forwarding the parent environment.
 */
import spawn from 'cross-spawn'
import { ipcMain } from 'electron'
import {
  aiComponentResponseSchema,
  type AiComponentRequest,
  type AiComponentResponse,
} from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { z } from 'zod'
import { Channel } from './ipc.js'

const CLAUDE_EXECUTABLE = 'claude'
const REQUEST_TIMEOUT_MS = 60_000
const STDERR_TAIL_CHARS = 2_000

// =====================
// === System prompt ===
// =====================

const ENSO_CHEAT_SHEET = `\
Enso is a functional, indentation-sensitive language for data work.
Syntax essentials:
- Assignment: \`name = expression\`. The last expression in a block is its value.
- Method chain: \`table.filter predicate . sort ["date"]\`.
- Lambda: \`x -> x + 1\`.
- Text literals use double quotes: "hello".
- Comments start with \`#\` and are rare in generated code.
- Blocks: indented lines belonging to the same statement.
- Qualified names: \`Standard.Base.Data.Vector.Vector.new\`.
Common stdlib entry points (Standard.Base / Standard.Table):
- \`Vector.new count fn\`, \`Vector.filter\`, \`Vector.map\`, \`Vector.reduce\`.
- \`Table.filter\`, \`Table.select_columns\`, \`Table.sort\`, \`Table.aggregate\`.
- \`Text.contains\`, \`Text.starts_with\`, \`Text.split\`.
- \`Data.read path\`, \`Data.write path value\`.`

const SYSTEM_PROMPT = `\
You generate the body of a User Defined Component in Enso — a small block of Enso code that takes one input binding and produces one output value.

${ENSO_CHEAT_SHEET}

You will receive:
- An input binding identifier (the parameter the block should operate on).
- Its Enso type name, when known.
- A natural-language description of what the block should do.

You must return a JSON object matching the supplied schema:
- \`body\`: a string containing the Enso block. Every line belongs to the block; no leading or trailing blank lines. The final line's expression is the value the block returns. Do NOT include the function signature, the \`=\` sign, or any \`main\` / module wrapper — only the body lines.

Rules:
- Reference the input binding by its identifier; do not invent another name for it.
- At most one method call per line; split chained calls across lines using intermediate bindings. This keeps each step readable as a graph node.
- The final line must be a single identifier — the binding that holds the result. Do not put an expression on the last line; assign it to a name first and reference that name.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.`

// JSON Schema passed to the CLI's `--json-schema` flag. Must stay in sync with
// `aiComponentResponseSchema` in `enso-common/src/ai.ts`; when the zod schema grows a
// field, mirror it here.
const RESPONSE_SCHEMA = {
  type: 'object',
  properties: {
    body: { type: 'string' },
  },
  required: ['body'],
  additionalProperties: false,
} as const

function parseJsonSafe(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

// Claude CLI's `--output-format json` envelope. With `--json-schema` active the CLI puts
// the validated payload in `structured_output`; older releases (or runs without the flag)
// put it in `result`, which may be either a pre-decoded object or a stringified JSON. Each
// field parses straight to `AiComponentResponse | null`: `.catch(null)` so a mismatched
// field never fails the whole envelope, and the caller picks the first non-null candidate.
const cliEnvelopeSchema = z.object({
  // eslint-disable-next-line camelcase
  structured_output: aiComponentResponseSchema.nullable().catch(null),
  result: z
    .preprocess(
      (input) => (typeof input === 'string' ? parseJsonSafe(input) : input),
      aiComponentResponseSchema,
    )
    .nullable()
    .catch(null),
})

// =================
// === Prompt IO ===
// =================

function buildUserPrompt(request: AiComponentRequest): string {
  const { prompt, context } = request
  const typeLine =
    context.sourceTypeName ?
      `Input binding type: ${context.sourceTypeName}`
    : 'Input binding type: unknown'
  return `Input binding identifier: ${context.sourceIdentifier}
${typeLine}

User request: ${prompt}`
}

// ======================
// === CLI invocation ===
// ======================

interface CliOutcome {
  stdout: string
  stderr: string
  exitCode: number | null
  spawnError?: NodeJS.ErrnoException
}

// TODO[ao]: Here add `--allowedTools Read Glob Grep` plus `--add-dir <stdlibRoot>
// --add-dir <projectSrcRoot>` once stdlib and project paths are threaded through the
// request.

// `--setting-sources ''` keeps the invocation hermetic (no user settings, plugins,
// or `CLAUDE.md` discovery) without touching auth.
// `--bare` is deliberately *not* used: it disables OAuth/keychain.
function buildCliArgs(): string[] {
  return [
    '--print',
    '--output-format',
    'json',
    '--json-schema',
    JSON.stringify(RESPONSE_SCHEMA),
    '--system-prompt',
    SYSTEM_PROMPT,
    '--tools',
    '',
    '--setting-sources',
    '',
    '--no-session-persistence',
  ]
}

function runClaude(
  args: readonly string[],
  stdinPayload: string,
  signal: AbortSignal,
): Promise<CliOutcome> {
  return new Promise((resolve) => {
    // `cross-spawn` (not `node:child_process`) so npm-installed Claude Code on Windows works:
    // npm wraps the package's bin entry as `claude.cmd`, which Node's `spawn` won't resolve
    // without `shell: true`. cross-spawn handles `.cmd`/`.ps1` lookup and quoting on Windows
    // and is a no-op on POSIX.
    const child = spawn(CLAUDE_EXECUTABLE, args, {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: process.env,
    })
    const stdoutChunks: Buffer[] = []
    const stderrChunks: Buffer[] = []
    let spawnError: NodeJS.ErrnoException | undefined
    let settled = false
    const settle = () => {
      if (settled) return
      settled = true
      const outcome: CliOutcome = {
        stdout: Buffer.concat(stdoutChunks).toString('utf8'),
        stderr: Buffer.concat(stderrChunks).toString('utf8'),
        exitCode: child.exitCode,
      }
      if (spawnError) outcome.spawnError = spawnError
      resolve(outcome)
    }

    child.stdout?.on('data', (chunk: Buffer) => stdoutChunks.push(chunk))
    child.stderr?.on('data', (chunk: Buffer) => stderrChunks.push(chunk))
    child.on('error', (err) => {
      spawnError = err as NodeJS.ErrnoException
    })
    child.on('close', settle)

    signal.addEventListener(
      'abort',
      () => {
        child.kill('SIGTERM')
      },
      { once: true },
    )

    if (child.stdin) {
      // Swallow EPIPE etc. — the 'close' handler still resolves with the captured state.
      child.stdin.on('error', (err) => {
        spawnError ??= err as NodeJS.ErrnoException
      })
      child.stdin.end(stdinPayload)
    }
  })
}

// =======================
// === Output parsing ===
// =======================

/** Parse the CLI's stdout into an {@link AiComponentResponse}, or a structured error. */
function parseCliResponse(stdout: string): Result<AiComponentResponse> {
  const envelopeJson = parseJsonSafe(stdout)
  if (envelopeJson == null) return Err('Claude agent produced malformed JSON on stdout')
  const envelope = cliEnvelopeSchema.safeParse(envelopeJson)
  if (!envelope.success) return Err('Claude agent stdout did not match the expected envelope')
  const payload = envelope.data.structured_output ?? envelope.data.result
  if (payload == null) return Err('Claude agent returned a result without a valid `body` field')
  return Ok(payload)
}

function truncateStderr(stderr: string): string {
  const trimmed = stderr.trim()
  if (trimmed.length <= STDERR_TAIL_CHARS) return trimmed
  return `…${trimmed.slice(-STDERR_TAIL_CHARS)}`
}

// ====================
// === Public entry ===
// ====================

/** Run the local `claude` CLI to produce a User Defined Component body. */
export async function generateAiComponent(
  request: AiComponentRequest,
): Promise<Result<AiComponentResponse>> {
  const abortController = new AbortController()
  const timeout = setTimeout(() => abortController.abort(), REQUEST_TIMEOUT_MS)
  try {
    const cli = await runClaude(buildCliArgs(), buildUserPrompt(request), abortController.signal)
    if (cli.spawnError) {
      if (cli.spawnError.code === 'ENOENT') {
        return Err(
          `'${CLAUDE_EXECUTABLE}' executable not found on PATH — install Claude Code to use the AI node feature`,
        )
      }
      return Err(`Failed to spawn '${CLAUDE_EXECUTABLE}': ${cli.spawnError.message}`)
    }
    if (abortController.signal.aborted) {
      return Err(`Claude agent timed out after ${REQUEST_TIMEOUT_MS}ms`)
    }
    if (cli.exitCode !== 0) {
      const tail = truncateStderr(cli.stderr)
      const detail = tail ? `: ${tail}` : ''
      return Err(`'${CLAUDE_EXECUTABLE}' exited with code ${cli.exitCode}${detail}`)
    }
    return parseCliResponse(cli.stdout)
  } finally {
    clearTimeout(timeout)
  }
}

// ======================
// === Startup probe ===
// ======================

// Best-effort check that `claude` is reachable. Non-blocking: startup continues even if the
// probe fails, because the first real IPC call surfaces the error to the renderer anyway.
function probeClaudeVersion(): void {
  let probe
  try {
    probe = spawn(CLAUDE_EXECUTABLE, ['--version'], {
      stdio: ['ignore', 'pipe', 'pipe'],
      env: process.env,
    })
  } catch (err) {
    console.warn(`[AI] could not spawn '${CLAUDE_EXECUTABLE} --version' probe:`, err)
    return
  }
  const stdoutChunks: string[] = []
  const stderrChunks: string[] = []
  probe.stdout?.on('data', (chunk: Buffer) => stdoutChunks.push(chunk.toString('utf8')))
  probe.stderr?.on('data', (chunk: Buffer) => stderrChunks.push(chunk.toString('utf8')))
  probe.on('error', (err) => {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      console.warn(
        `[AI] '${CLAUDE_EXECUTABLE}' not found on PATH; AI node generation will fail until Claude Code is installed.`,
      )
      return
    }
    console.warn(`[AI] '${CLAUDE_EXECUTABLE} --version' probe failed:`, err.message)
  })
  probe.on('close', (exitCode) => {
    if (exitCode === 0) {
      console.info(`[AI] '${CLAUDE_EXECUTABLE}' CLI available: ${stdoutChunks.join('').trim()}`)
    } else if (exitCode != null) {
      console.warn(
        `[AI] '${CLAUDE_EXECUTABLE} --version' exited ${exitCode}: ${stderrChunks.join('').trim()}`,
      )
    }
  })
}

// ===================
// === IPC binding ===
// ===================

/** Register the {@link Channel.generateAiComponent} IPC handler. */
export function initClaudeAgentIpc() {
  probeClaudeVersion()
  ipcMain.handle(Channel.generateAiComponent, async (_event, request: AiComponentRequest) =>
    generateAiComponent(request),
  )
}
