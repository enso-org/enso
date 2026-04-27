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
You generate a top-level User Defined Component in Enso — a function definition plus the call that places it inside an existing method on the user's graph.

${ENSO_CHEAT_SHEET}

You will receive:
- The Enso method the call site lives in (its name and full source).
- The source binding the user dropped into the AI prompt (identifier and Enso type, when known) — this is the value they want to operate on.
- Other identifiers already in scope in that method, with their Enso types when known. You may reference any of them.
- A natural-language description of what the new component should do.

You must return a JSON object matching the supplied schema, with these four fields:
- \`functionName\`: snake_case identifier for the new top-level function. It must not collide with an identifier already used in the surrounding method or with a name visible in the supplied method source. Pick something descriptive of what the function does.
- \`argumentNames\`: list of identifiers the function takes as parameters. Each one must be an in-scope binding from the supplied list (or the source binding identifier). The same identifier doubles as both the parameter name in the function signature and the value passed at the call site, so the names you list here are also the names the call expression must pass. Always include the source binding when the function operates on it; add other in-scope identifiers only when the function actually uses them.
- \`body\`: the function body, as a string. Every line belongs to the body; no leading or trailing blank lines. Reference the parameters by the names you listed in \`argumentNames\`. The final line must be a single identifier — the binding that holds the result. Do not include the function signature, the \`=\` sign, or any module wrapper.
- \`callExpression\`: the Enso expression placed in the user's method, of the form \`Main.<functionName> <arg1> <arg2> …\` where the args are exactly the identifiers from \`argumentNames\`, in the same order. Do not use other in-scope identifiers here — bind them through \`argumentNames\` instead.

Rules:
- At most one method call per line in \`body\`; split chained calls across lines using intermediate bindings. This keeps each step readable as a graph node.
- The final line of \`body\` must be a single identifier — assign expressions to a name first and reference that name.
- Do not introduce parameters that aren't actually used inside \`body\`.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.`

// JSON Schema passed to the CLI's `--json-schema` flag. Must stay in sync with
// `aiComponentResponseSchema` in `enso-common/src/ai.ts`; when the zod schema grows a
// field, mirror it here.
const RESPONSE_SCHEMA = {
  type: 'object',
  properties: {
    functionName: { type: 'string' },
    argumentNames: { type: 'array', items: { type: 'string' } },
    body: { type: 'string' },
    callExpression: { type: 'string' },
  },
  required: ['functionName', 'argumentNames', 'body', 'callExpression'],
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

function formatBinding(identifier: string, typeName: string | undefined): string {
  return typeName ? `- ${identifier} : ${typeName}` : `- ${identifier} : (type unknown)`
}

function buildUserPrompt(request: AiComponentRequest): string {
  const { prompt, context } = request
  const otherBindings = context.inScopeBindings
    .filter((binding) => binding.identifier !== context.sourceIdentifier)
    .map((binding) => formatBinding(binding.identifier, binding.typeName))
  const otherBindingsSection =
    otherBindings.length > 0 ?
      `Other in-scope bindings:\n${otherBindings.join('\n')}`
    : 'Other in-scope bindings: (none)'
  return `Current method: ${context.currentMethodName}
Current method source:
\`\`\`
${context.currentMethodCode}
\`\`\`

Source binding (the value the user wants to operate on):
${formatBinding(context.sourceIdentifier, context.sourceTypeName)}

${otherBindingsSection}

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
