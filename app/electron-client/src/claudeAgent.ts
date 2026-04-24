/**
 * @file Electron-main-side adapter that shells out to the user-installed `claude` CLI
 * executable to generate the body of a User Defined Component from a natural-language
 * prompt. Exposed to the renderer via IPC; the renderer never invokes the CLI directly.
 *
 * Authentication rides on whatever the `claude` CLI is already configured with (OAuth,
 * keychain, `ANTHROPIC_API_KEY`, or subscription token). The main process does not require
 * or read the API key beyond forwarding the parent environment.
 */
import { ipcMain } from 'electron'
import type { AiComponentRequest, AiComponentResponse } from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { spawn } from 'node:child_process'
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
- Prefer simple, readable code; break long pipelines over multiple lines using intermediate bindings.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.`

const RESPONSE_SCHEMA = {
  type: 'object',
  properties: {
    body: { type: 'string' },
  },
  required: ['body'],
  additionalProperties: false,
} as const

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

// `--tools ''` disables all built-in tools, matching Step 1's SDK `allowedTools: []`. Step 6
// will replace this with `--allowedTools Read Glob Grep` plus `--add-dir <stdlibRoot>
// --add-dir <projectSrcRoot>` once stdlib and project paths are threaded through the
// request. `--setting-sources ''` keeps the invocation hermetic (no user settings, plugins,
// or `CLAUDE.md` discovery) without touching auth. `--bare` is deliberately *not* used: it
// disables OAuth/keychain and would re-introduce the `ANTHROPIC_API_KEY` requirement we
// just dropped.
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

function parseJsonSafe(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

// With `--json-schema` active, the CLI places the model's structured output in the
// envelope's `result` field. Current releases stringify it; we also accept an already-
// decoded object for forward-compatibility.
function extractPayload(envelope: unknown): unknown {
  if (envelope == null || typeof envelope !== 'object' || !('result' in envelope)) return null
  const result = (envelope as { result: unknown }).result
  if (typeof result === 'string') return parseJsonSafe(result)
  return result
}

function extractBody(payload: unknown): string | null {
  if (
    payload != null &&
    typeof payload === 'object' &&
    'body' in payload &&
    typeof payload.body === 'string'
  ) {
    return payload.body
  }
  return null
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
    const envelope = parseJsonSafe(cli.stdout)
    if (envelope == null) {
      return Err('Claude agent produced malformed JSON on stdout')
    }
    const body = extractBody(extractPayload(envelope))
    if (body == null) {
      return Err('Claude agent returned a result without a valid `body` field')
    }
    return Ok({ body })
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
