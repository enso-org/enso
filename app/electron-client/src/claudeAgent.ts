/**
 * @file Electron-main-side adapter that drives a long-lived `claude` CLI subprocess to generate
 * the body of a User Defined Component from a natural-language prompt. Exposed to the renderer
 * via IPC; the renderer never invokes the CLI directly.
 *
 * The session is spawned eagerly (but non-blockingly) at app launch, primed with a small
 * acknowledgment turn so the system prompt is ingested before the first real request, and
 * serialized via a per-process FIFO queue. Crashes are logged and the child is respawned in the
 * background; per-request timeouts return errors without killing the still-warm child.
 *
 * Authentication rides on whatever the `claude` CLI is already configured with. The main process
 * does not require or read the API key beyond forwarding the parent environment.
 */
import spawn from 'cross-spawn'
import { ipcMain } from 'electron'
import {
  aiComponentResponseSchema,
  type AiComponentIpcReply,
  type AiComponentRequest,
  type AiComponentResponse,
  type RequestUsage,
} from 'enso-common/src/ai'
import { AsyncQueue } from 'enso-common/src/utilities/async'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { type ChildProcess } from 'node:child_process'
import readline from 'node:readline'
import { Channel } from './ipc.js'

const CLAUDE_EXECUTABLE = 'claude'
const REQUEST_TIMEOUT_MS = 120_000
const PRIMING_TIMEOUT_MS = 60_000
const STDERR_TAIL_CHARS = 2_000
const RESPAWN_WINDOW_MS = 30_000
const MAX_RESPAWNS_IN_WINDOW = 3
const PRIMING_PROMPT =
  'Acknowledge readiness with the single word READY. This is a session warm-up; do not return JSON.'

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

You must return a JSON object with these four fields and nothing else (no prose, no code fences, no leading or trailing whitespace):
- \`functionName\`: snake_case identifier for the new top-level function. It must not collide with an identifier already used in the surrounding method or with a name visible in the supplied method source. Pick something descriptive of what the function does.
- \`argumentNames\`: parameter names in the function signature, in declaration order. Pick names that describe each parameter's role inside the function — they do *not* have to match any in-scope identifier and they are the names you reference inside \`body\`. Only declare parameters that \`body\` actually uses.
- \`body\`: the function body, as a string. Every line belongs to the body; no leading or trailing blank lines. Reference the parameters by the names you listed in \`argumentNames\`. The final line must be a single identifier — the binding that holds the result. Do not include the function signature, the \`=\` sign, or any module wrapper.
- \`callArguments\`: Enso expressions passed at the call site, one per parameter and in the same order as \`argumentNames\`. Each entry is usually just an in-scope identifier (the source binding or one of the other in-scope bindings), but any single Enso expression is accepted. The renderer wraps them as \`Main.<functionName> <callArguments[0]> <callArguments[1]> ...\`. Always pass the source binding when the function operates on it; pass other in-scope identifiers only when the function uses them.

Rules:
- At most one method call per line in \`body\`; split chained calls across lines using intermediate bindings. This keeps each step readable as a graph node.
- The final line of \`body\` must be a single identifier — assign expressions to a name first and reference that name.
- \`argumentNames\` and \`callArguments\` must have the same length.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.

If the user message is a session warm-up and the request is not for a component, reply briefly in plain text. Otherwise, every reply must be the JSON object described above.`

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
  const otherBindingsList = otherBindings.length > 0 ? otherBindings.join('\n') : '(none)'
  return `Current method: ${context.currentMethodName}
Current method source:
\`\`\`
${context.currentMethodCode}
\`\`\`

Source binding (the value the user wants to operate on):
${formatBinding(context.sourceIdentifier, context.sourceTypeName)}

Other in-scope bindings:
${otherBindingsList}

User request: ${prompt}`
}

function parseJsonSafe(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

function truncateStderr(stderr: string): string {
  const trimmed = stderr.trim()
  if (trimmed.length <= STDERR_TAIL_CHARS) return trimmed
  return `…${trimmed.slice(-STDERR_TAIL_CHARS)}`
}

// ====================================
// === Stream-json wire format glue ===
// ====================================

// Probe-confirmed envelope shape. See app/electron-client/CLAUDE.md for the discovery notes.
// Note: `--verbose` is required by the CLI alongside `--output-format stream-json` (without it
// the child exits 1 immediately with "When using --print, --output-format=stream-json requires
// --verbose"). The extra system/init and rate_limit_event envelopes it emits are filtered in
// `onStdoutLine`.
function streamJsonArgs(): string[] {
  return [
    '-p',
    '--input-format',
    'stream-json',
    '--output-format',
    'stream-json',
    '--verbose',
    '--system-prompt',
    SYSTEM_PROMPT,
    '--tools',
    '',
    '--setting-sources',
    '',
    '--no-session-persistence',
  ]
}

function userTurnLine(content: string): string {
  return JSON.stringify({ type: 'user', message: { role: 'user', content } }) + '\n'
}

interface RawTokenUsage {
  input_tokens?: number
  output_tokens?: number
}

interface TurnOutcome {
  state: 'completed' | 'crash'
  text: string
  usage: RawTokenUsage | null
  errorReason?: string
}

interface PendingTurn {
  resolve: (outcome: TurnOutcome) => void
  textChunks: string[]
}

interface Deferred<T> {
  promise: Promise<T>
  resolve: (value: T) => void
  reject: (error: unknown) => void
}

function makeDeferred<T>(): Deferred<T> {
  let resolve!: (value: T) => void
  let reject!: (error: unknown) => void
  const promise = new Promise<T>((res, rej) => {
    resolve = res
    reject = rej
  })
  return { promise, resolve, reject }
}

// ============================
// === ClaudeAgentSession ===
// ============================

/**
 * Owns a single long-lived `claude` subprocess. Construction kicks off `spawn()` and `prime()`
 * without awaiting either, so callers can run the constructor synchronously during Electron
 * startup. `runRequest` awaits priming via `this.ready`.
 */
export class ClaudeAgentSession {
  private child: ChildProcess | null = null
  private readyDeferred: Deferred<void> = makeDeferred()
  private readonly queue = new AsyncQueue<void>(Promise.resolve())
  private pending: PendingTurn | null = null
  private contextBytes = 0
  private stderrTail = ''
  private respawnTimes: number[] = []
  private respawnSuspended = false
  private disposed = false

  /** Spawns the underlying `claude` child eagerly; priming continues in the background. */
  constructor() {
    this.spawn()
  }

  /** Resolves once the current child has spawned and accepted a priming turn. */
  get ready(): Promise<void> {
    return this.readyDeferred.promise
  }

  /** Run an AI component request through the long-lived session. */
  runRequest(request: AiComponentRequest): Promise<AiComponentIpcReply> {
    return new Promise<AiComponentIpcReply>((resolveOuter) => {
      this.queue.pushTask(async () => {
        if (this.disposed) {
          resolveOuter({ result: Err('Claude agent has been shut down'), usage: null })
          return
        }
        if (this.respawnSuspended && (this.child == null || this.child.exitCode != null)) {
          // Allow one respawn attempt per IPC call after the crash-loop guard tripped. We do NOT
          // clear `respawnTimes` — keeping the recent failures means another quick crash trips the
          // guard again immediately, instead of granting an infinite retry stream.
          this.respawnSuspended = false
          this.spawn()
        }
        try {
          await this.ready
        } catch (err) {
          resolveOuter({
            result: Err(this.formatNotReadyError(err)),
            usage: null,
          })
          return
        }
        const turn = await this.runOneTurn(buildUserPrompt(request), REQUEST_TIMEOUT_MS)
        resolveOuter(this.replyFromTurn(turn))
      })
    })
  }

  /** Request graceful shutdown of the child and reject any pending work. */
  shutdown(): void {
    if (this.disposed) return
    this.disposed = true
    if (this.pending) {
      const pending = this.pending
      this.pending = null
      pending.resolve({
        state: 'crash',
        text: '',
        usage: null,
        errorReason: 'Claude agent shutting down',
      })
    }
    if (this.child && this.child.exitCode == null) {
      this.child.kill('SIGTERM')
    }
  }

  // -------------- private --------------

  private spawn(): void {
    if (this.disposed) return
    this.readyDeferred = makeDeferred()
    // Attach a no-op rejection handler so that a crash mid-priming doesn't surface as an
    // unhandled rejection when no `runRequest` happens to be awaiting `ready` at the time.
    // Awaiters that arrive later attach their own .then/.catch and still observe the rejection.
    this.readyDeferred.promise.catch(() => undefined)
    this.contextBytes = Buffer.byteLength(SYSTEM_PROMPT, 'utf8')
    this.stderrTail = ''
    this.pending = null

    let child: ChildProcess
    try {
      // `cross-spawn` (not `node:child_process`) so npm-installed Claude Code on Windows works:
      // npm wraps the package's bin entry as `claude.cmd`, which Node's `spawn` won't resolve
      // without `shell: true`. cross-spawn handles `.cmd`/`.ps1` lookup on Windows, no-op on POSIX.
      child = spawn(CLAUDE_EXECUTABLE, streamJsonArgs(), {
        stdio: ['pipe', 'pipe', 'pipe'],
        env: process.env,
      })
    } catch (err) {
      this.readyDeferred.reject(err)
      return
    }
    this.child = child

    if (child.stdout) {
      const rl = readline.createInterface({ input: child.stdout })
      rl.on('line', (line) => this.onStdoutLine(line))
    }
    child.stderr?.on('data', (chunk: Buffer) => this.appendStderr(chunk.toString('utf8')))
    child.on('error', (err) => {
      this.handleChildEnded(`spawn error: ${(err as NodeJS.ErrnoException).message}`)
    })
    child.on('exit', (code, signal) => {
      if (this.disposed) return
      this.handleChildEnded(`exited with code=${code} signal=${signal ?? 'null'}`)
    })

    void this.prime().then(
      () => this.readyDeferred.resolve(),
      (err) => this.readyDeferred.reject(err),
    )
  }

  private async prime(): Promise<void> {
    const outcome = await this.runOneTurn(PRIMING_PROMPT, PRIMING_TIMEOUT_MS)
    if (outcome.state !== 'completed') {
      throw new Error(`priming turn ${outcome.state}: ${outcome.errorReason ?? '(no detail)'}`)
    }
    if (!outcome.text.trim()) {
      throw new Error('priming turn produced no assistant text')
    }
  }

  private runOneTurn(content: string, timeoutMs: number): Promise<TurnOutcome> {
    return new Promise<TurnOutcome>((resolveTurn) => {
      const child = this.child
      if (!child || child.exitCode != null || !child.stdin || child.stdin.destroyed) {
        resolveTurn({
          state: 'crash',
          text: '',
          usage: null,
          errorReason: 'child process is not alive',
        })
        return
      }
      const line = userTurnLine(content)
      this.contextBytes += Buffer.byteLength(line, 'utf8')
      const pending: PendingTurn = {
        resolve: (outcome) => {
          if (timeoutHandle != null) clearTimeout(timeoutHandle)
          resolveTurn(outcome)
        },
        textChunks: [],
      }
      this.pending = pending
      const timeoutHandle = setTimeout(() => {
        if (this.pending !== pending) return
        // Drop the pending claim so subsequent stdout for this turn is discarded; the runtime
        // does NOT kill the child, because the next request will reuse it.
        this.pending = null
        pending.resolve({
          state: 'crash',
          text: '',
          usage: null,
          errorReason: `timed out after ${timeoutMs}ms`,
        })
      }, timeoutMs)
      child.stdin.write(line, (err) => {
        if (!err) return
        if (this.pending === pending) {
          this.pending = null
          pending.resolve({
            state: 'crash',
            text: '',
            usage: null,
            errorReason: `stdin write failed: ${err.message}`,
          })
        }
      })
    })
  }

  private onStdoutLine(line: string): void {
    const env = parseJsonSafe(line)
    if (env == null || typeof env !== 'object') return
    const type = (env as { type?: unknown }).type
    if (type === 'system' || type === 'rate_limit_event' || type === 'user') return
    if (type === 'assistant') {
      this.captureAssistantContent(env)
      return
    }
    if (type === 'result') {
      this.resolveTerminal(env)
    }
  }

  private captureAssistantContent(env: object): void {
    if (!this.pending) return
    const message = (env as { message?: unknown }).message
    if (!message || typeof message !== 'object') return
    const content = (message as { content?: unknown }).content
    if (!Array.isArray(content)) return
    for (const block of content) {
      if (
        block != null &&
        typeof block === 'object' &&
        (block as { type?: unknown }).type === 'text' &&
        typeof (block as { text?: unknown }).text === 'string'
      ) {
        const text = (block as { text: string }).text
        this.pending.textChunks.push(text)
        this.contextBytes += Buffer.byteLength(text, 'utf8')
      }
    }
  }

  private resolveTerminal(env: object): void {
    if (!this.pending) return
    const pending = this.pending
    this.pending = null
    const result = (env as { result?: unknown }).result
    const usageRaw = (env as { usage?: unknown }).usage
    const text =
      typeof result === 'string' && result.length > 0 ? result : pending.textChunks.join('')
    pending.resolve({
      state: 'completed',
      text,
      usage: usageRaw != null && typeof usageRaw === 'object' ? (usageRaw as RawTokenUsage) : null,
    })
  }

  private handleChildEnded(reason: string): void {
    const stderrTail = truncateStderr(this.stderrTail)
    const detail = stderrTail ? `: ${stderrTail}` : ''
    console.warn(`[AI] claude process crashed (${reason})${detail}`)
    if (this.pending) {
      const pending = this.pending
      this.pending = null
      pending.resolve({
        state: 'crash',
        text: '',
        usage: null,
        errorReason: reason,
      })
    }
    // Reject the priming promise so any task awaiting `ready` fails fast — the next task in the
    // queue will see a fresh deferred created by the upcoming spawn() call (if respawn is allowed).
    this.readyDeferred.reject(new Error(reason))
    this.child = null
    if (this.disposed) return

    const now = Date.now()
    this.respawnTimes = this.respawnTimes.filter((t) => now - t <= RESPAWN_WINDOW_MS)
    this.respawnTimes.push(now)
    if (this.respawnTimes.length >= MAX_RESPAWNS_IN_WINDOW) {
      this.respawnSuspended = true
      console.warn(
        `[AI] claude crashed ${this.respawnTimes.length} times in ${RESPAWN_WINDOW_MS}ms; suspending auto-respawn until the next request.`,
      )
      return
    }
    this.spawn()
  }

  private appendStderr(chunk: string): void {
    this.stderrTail = (this.stderrTail + chunk).slice(-STDERR_TAIL_CHARS)
  }

  private replyFromTurn(turn: TurnOutcome): AiComponentIpcReply {
    const usage = this.snapshotUsage(turn.usage)
    if (turn.state !== 'completed') {
      const reason = turn.errorReason ?? 'claude turn failed'
      return { result: Err(`Claude agent: ${reason}`), usage }
    }
    if (!turn.text.trim()) {
      return { result: Err('Claude agent returned an empty reply'), usage }
    }
    const parsedJson = parseJsonSafe(turn.text)
    if (parsedJson == null) {
      return { result: Err('Claude agent reply was not valid JSON'), usage }
    }
    const parsed = aiComponentResponseSchema.safeParse(parsedJson)
    if (!parsed.success) {
      return {
        result: Err('Claude agent reply did not match the expected component schema'),
        usage,
      }
    }
    return { result: Ok(parsed.data), usage }
  }

  private snapshotUsage(raw: RawTokenUsage | null): RequestUsage | null {
    if (!raw) return null
    return {
      inputTokens: raw.input_tokens ?? 0,
      outputTokens: raw.output_tokens ?? 0,
      contextBytes: this.contextBytes,
    }
  }

  private formatNotReadyError(err: unknown): string {
    const errno = err as NodeJS.ErrnoException | null
    if (errno?.code === 'ENOENT') {
      return `'${CLAUDE_EXECUTABLE}' executable not found on PATH — install Claude Code to use the AI node feature`
    }
    const message = errno?.message ?? String(err ?? 'unknown error')
    return `Claude agent is not ready: ${message}`
  }
}

// ====================
// === Module state ===
// ====================

let session: ClaudeAgentSession | null = null

/** Run the local `claude` CLI to produce a User Defined Component body. */
export async function generateAiComponent(
  request: AiComponentRequest,
): Promise<Result<AiComponentResponse>> {
  const reply = await generateAiComponentWithUsage(request)
  return reply.result
}

/**
 * Same as {@link generateAiComponent} but also returns the per-turn usage telemetry. This is what
 * the IPC handler exposes to the renderer; the renderer logs the usage line and forwards `result`
 * to its caller.
 */
export async function generateAiComponentWithUsage(
  request: AiComponentRequest,
): Promise<AiComponentIpcReply> {
  if (session == null) session = new ClaudeAgentSession()
  return session.runRequest(request)
}

/** Tear down the long-lived session. Wired to `app.on('before-quit', ...)` in `index.ts`. */
export function shutdownClaudeAgent(): void {
  session?.shutdown()
  session = null
}

// ===================
// === IPC binding ===
// ===================

/** Register the {@link Channel.generateAiComponent} IPC handler. */
export function initClaudeAgentIpc() {
  // Eager but non-blocking: spawning + priming happen in the background while Electron continues
  // its own startup. Subsequent IPC calls await `session.ready` before sending stdin.
  if (session == null) session = new ClaudeAgentSession()
  probeClaudeVersion()
  ipcMain.handle(Channel.generateAiComponent, async (_event, request: AiComponentRequest) =>
    generateAiComponentWithUsage(request),
  )
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
