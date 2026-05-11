/**
 * @file {@link ChildAgent} — driving one spawned `claude` subprocess. Wire format, spawn
 * flags, and CLI gotchas are documented in `electron-client/CLAUDE.md`.
 */
import spawn from 'cross-spawn'
import type { WebContents } from 'electron'
import type { AiProgressEvent } from 'enso-common/src/ai'
import { createDeferred, type Deferred } from 'enso-common/src/utilities/async'
import {
  ChildProcessHandle,
  WatchedChildProcess,
  type UnexpectedExitInfo,
} from 'enso-common/src/utilities/childProcess'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import readline from 'node:readline'
import { z } from 'zod'
import { Channel } from '../ipc.js'
import { buildPrimingPrompt, buildSystemPrompt } from './prompts.js'

const CLAUDE_EXECUTABLE = 'claude'
const STDERR_TAIL_CHARS = 2_000
const RESPAWN_WINDOW_MS = 30_000
const MAX_RESPAWNS_IN_WINDOW = 3

/** Synthetic request id used for the priming turn. */
export const PRIMING_REQUEST_ID = 'priming'

/**
 * Default priming-turn timeout (ms). Priming reads three CLAUDE.md files (top-level, Base,
 * Table) plus the entire `Standard.Image` source as a syntax demo (when stdlib is available);
 * ~12 tool calls before replying. 180s leaves comfortable headroom while staying well under
 * `REQUEST_TIMEOUT_MS`. The fallback path (no stdlib) only does the trivial "say READY" turn
 * and completes in well under a second, so the higher cap costs nothing on that path.
 */
export const PRIMING_TIMEOUT_MS = 180_000

/** SIGINT (graceful) to SIGTERM (force respawn) escalation window during cancellation. */
const CANCEL_SIGINT_TO_SIGTERM_MS = 2_000

/** Spawn-time configuration for one `claude` child. */
export interface ChildAgentConfig {
  /**
   * Bundled engine's `lib/Standard` directory, exposed to the agent via `--add-dir` plus the
   * `Read`/`Glob`/`Grep` tools. `undefined` disables stdlib filesystem access.
   */
  readonly stdlibRoot: string | undefined
  /**
   * Config file produced by the in-process MCP server, exposing the `evaluateExpression` tool.
   * `undefined` runs the agent without MCP (used by headless tests).
   */
  readonly mcpConfigPath: string | undefined
  /**
   * Optional label included in console warnings (e.g. "primary", "warming") so logs from a
   * session running two children stay disambiguated. Empty string means no label.
   */
  readonly logLabel?: string
}

/** Renderer + request id driving a turn. */
export interface ActiveRequest {
  readonly requestId: string
  readonly sender: WebContents
}

// ====================================
// === Stream-json wire format glue ===
// ====================================

// `--verbose` is required by the CLI alongside `--output-format stream-json` (without it the
// child exits 1 immediately with "When using --print, --output-format=stream-json requires
// --verbose"). The extra system/init and rate_limit_event envelopes the verbose output emits
// are filtered out by the schema-based parser in `onStdoutLine`.
//
// `--add-dir <stdlib>` plus `--allowedTools "Read,Glob,Grep"` lets the model browse the bundled
// standard library when it's unsure of an API. We pre-grant those tools (`--allowedTools`) so the
// CLI doesn't try to prompt — there's no UI to prompt against in `-p` mode.
function streamJsonArgs(config: ChildAgentConfig): string[] {
  // MCP tools are namespaced as `mcp__<server>__<tool>`. Conditional so a tool-less session
  // (e.g. headless tests) doesn't claim capabilities to the model.
  const allowedTools = [
    ...(config.stdlibRoot != null ? ['Read', 'Glob', 'Grep'] : []),
    ...(config.mcpConfigPath != null ? ['mcp__enso__evaluateExpression'] : []),
  ]
  return [
    '-p',
    '--input-format',
    'stream-json',
    '--output-format',
    'stream-json',
    '--verbose',
    '--system-prompt',
    buildSystemPrompt(config),
    ...(config.stdlibRoot != null ? ['--add-dir', config.stdlibRoot] : []),
    // `--strict-mcp-config` makes the CLI ignore project- and user-level MCP configs so the
    // session is hermetic and only sees our in-process server.
    ...(config.mcpConfigPath != null ?
      ['--mcp-config', config.mcpConfigPath, '--strict-mcp-config']
    : []),
    ...(allowedTools.length > 0 ? ['--allowedTools', allowedTools.join(',')] : []),
    '--setting-sources',
    '',
    '--no-session-persistence',
  ]
}

function userTurnLine(content: string): string {
  return JSON.stringify({ type: 'user', message: { role: 'user', content } }) + '\n'
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

// Probe-confirmed shape of the envelopes the CLI emits in stream-json mode. The format is
// undocumented (see https://github.com/anthropics/claude-code/issues/24594); discovery notes
// and per-turn emission order are in app/electron-client/CLAUDE.md.

/* eslint-disable camelcase -- mirrors the snake_case keys the CLI emits. */
const tokenUsageSchema = z.object({
  input_tokens: z.number().optional(),
  output_tokens: z.number().optional(),
  cache_creation_input_tokens: z.number().optional(),
  cache_read_input_tokens: z.number().optional(),
})
/* eslint-enable camelcase */

const assistantEnvelopeSchema = z.object({
  type: z.literal('assistant'),
  message: z.object({
    content: z.array(
      z.object({
        type: z.string(),
        text: z.string().optional(),
        name: z.string().optional(),
        input: z.unknown().optional(),
      }),
    ),
    usage: tokenUsageSchema.optional(),
  }),
})

const resultEnvelopeSchema = z.object({
  type: z.literal('result'),
  result: z.string().optional(),
  usage: tokenUsageSchema.optional(),
})

/** Token-usage shape the CLI surfaces on every assistant/result envelope. */
export type RawTokenUsage = z.infer<typeof tokenUsageSchema>

/** Outcome of a single user→model turn on one child. */
export interface TurnOutcome {
  state: 'completed' | 'crash'
  text: string
  /** Result envelope's `usage` (cost-side roll-up). */
  usage: RawTokenUsage | null
  /** `usage` from the final `assistant` envelope; `null` when none seen / CLI omitted it. */
  lastHopUsage: RawTokenUsage | null
  /** Number of `assistant` envelopes seen this turn. `0` for crash-before-first-response. */
  hopCount: number
  /**
   * Wall-clock ms from the moment we wrote the user turn to stdin until the turn settled
   * (completed, crashed, timed out, or was cancelled). `0` means the turn never started — the
   * child wasn't alive when {@link ChildAgent.runTurn} was called.
   */
  durationMs: number
  errorReason?: string
}

interface PendingTurn {
  /** Renderer-supplied id, or {@link PRIMING_REQUEST_ID} for priming. */
  readonly requestId: string
  resolve: (outcome: Omit<TurnOutcome, 'durationMs' | 'lastHopUsage' | 'hopCount'>) => void
  textChunks: string[]
  /** Pinned per turn (not per child) so crash/shutdown drop the slot for free; `null` for priming. */
  sender: WebContents | null
  /**
   * Most recent `assistant` envelope's `message.usage` we observed in this turn. Used as the
   * "current context window occupancy" signal at turn end. `null` when the CLI doesn't surface
   * per-envelope `usage`.
   */
  lastHopUsage: RawTokenUsage | null
  /** Number of `assistant` envelopes seen this turn. */
  hopCount: number
}

// ============================
// === ChildAgent           ===
// ============================

/**
 * Owns one `claude` subprocess and the per-child glue around it: priming, stream-json
 * envelope parsing, single-turn execution, cancellation signaling, and crash/respawn
 * bookkeeping (with a crash-loop guard that suspends auto-respawn after repeated failures).
 */
export class ChildAgent {
  private readonly config: ChildAgentConfig
  private readonly watcher: WatchedChildProcess
  private readyDeferred: Deferred<void> = createDeferred()
  private pending: PendingTurn | null = null
  private stderrTail = ''
  private disposed = false
  /**
   * Empty temp dir used as the spawned `claude` process's cwd. Without it the child inherits
   * Electron's cwd and the agent's `Read` tool can reach arbitrary paths under it (user home,
   * `/tmp`, ...). Pairing an empty cwd with `--add-dir <stdlibRoot>` confines `Read`/`Glob`/
   * `Grep` to the stdlib, forcing the agent to use `evaluateExpression` for any user-data
   * inspection — which is the right boundary anyway, since user data lives in the engine and
   * is only correctly observable through the LS.
   */
  private readonly sandboxCwd: string
  private isReadyResolved = false

  /** Spawn the child eagerly and kick off the priming turn in the background. */
  constructor(config: ChildAgentConfig) {
    this.config = config
    this.sandboxCwd = fs.mkdtempSync(path.join(os.tmpdir(), 'enso-claude-cwd-'))
    // Attach a no-op rejection handler so that a crash mid-priming doesn't surface as an
    // unhandled rejection when no caller happens to be awaiting `ready` at the time. Awaiters
    // that arrive later attach their own .then/.catch and still observe the rejection.
    this.observeReadyDeferred(this.readyDeferred)
    this.watcher = new WatchedChildProcess(
      // `cross-spawn` (not `node:child_process`) so npm-installed Claude Code on Windows works:
      // npm wraps the package's bin entry as `claude.cmd`, which Node's `spawn` won't resolve
      // without `shell: true`. cross-spawn handles `.cmd`/`.ps1` lookup on Windows, no-op on POSIX.
      () =>
        spawn(CLAUDE_EXECUTABLE, streamJsonArgs(this.config), {
          stdio: ['pipe', 'pipe', 'pipe'],
          env: process.env,
          cwd: this.sandboxCwd,
        }),
      {
        onChildStarted: this.onChildStarted.bind(this),
        onUnexpectedExit: this.onUnexpectedExit.bind(this),
        crashLoopLimit: { maxCrashes: MAX_RESPAWNS_IN_WINDOW, windowMs: RESPAWN_WINDOW_MS },
      },
    )
    // Synchronous spawner failures (e.g. cross-spawn rejecting on a missing CLI) are reported
    // via firstSpawn so awaiters of `this.ready` see the underlying error rather than hanging.
    this.watcher.firstSpawn.catch((err) => {
      this.readyDeferred.reject(err)
    })
  }

  /** Resolves once the current child has spawned and accepted the priming turn. */
  get ready(): Promise<void> {
    return this.readyDeferred.promise
  }

  /**
   * Synchronous query: has the most recent `readyDeferred` resolved? Useful for the session
   * to ask "is warming primed yet?" without `await`ing.
   */
  get isReady(): boolean {
    return this.isReadyResolved
  }

  /**
   * The renderer + request id driving the in-flight turn, or `null` between turns / when the
   * renderer was destroyed / during priming (priming has `sender == null`).
   */
  get activeRequest(): ActiveRequest | null {
    const pending = this.pending
    if (pending == null) return null
    const sender = pending.sender
    if (sender == null || sender.isDestroyed()) return null
    return { requestId: pending.requestId, sender }
  }

  /** Whether the underlying watcher's auto-respawn has been suspended (crash-loop guard). */
  get respawnSuspended(): boolean {
    return this.watcher.respawnSuspended
  }

  /** Whether the underlying child process is currently alive. */
  get alive(): boolean {
    return this.watcher.current?.alive === true
  }

  /**
   * Manually spawn a fresh child after the auto-respawn was suspended. The watcher's
   * recent-exits buffer is preserved across `respawn()`, so a quick re-crash trips the guard
   * again and we don't loop indefinitely.
   */
  async respawn(): Promise<void> {
    this.swapInReadyDeferred()
    await this.watcher.respawn()
  }

  /**
   * Run one turn. Writes the user content to stdin and awaits the `result` envelope (or a
   * crash/timeout). The caller is responsible for serializing turns — this class never
   * cancels a previous turn implicitly.
   */
  runTurn(
    content: string,
    timeoutMs: number,
    sender: WebContents | null,
    requestId: string,
  ): Promise<TurnOutcome> {
    return new Promise<TurnOutcome>((resolveTurn) => {
      const handle = this.watcher.current
      const child = handle?.child
      if (!handle?.alive || !child?.stdin || child.stdin.destroyed) {
        resolveTurn({
          state: 'crash',
          text: '',
          usage: null,
          lastHopUsage: null,
          hopCount: 0,
          durationMs: 0,
          errorReason: 'child process is not alive',
        })
        return
      }
      const line = userTurnLine(content)
      const startedAt = performance.now()
      const pending: PendingTurn = {
        requestId,
        resolve: (outcome) => {
          if (timeoutHandle != null) clearTimeout(timeoutHandle)
          const durationMs = Math.max(0, Math.round(performance.now() - startedAt))
          resolveTurn({
            ...outcome,
            durationMs,
            lastHopUsage: pending.lastHopUsage,
            hopCount: pending.hopCount,
          })
        },
        textChunks: [],
        sender,
        lastHopUsage: null,
        hopCount: 0,
      }
      this.pending = pending
      // Emit `started` after `pending` is set so `emitProgress` can find the sender, and before
      // the stdin write so the renderer flips queued→running close to when the prompt actually
      // begins traveling toward the API.
      this.emitProgress({ requestId, kind: 'started' })
      const timeoutHandle = setTimeout(() => {
        if (this.pending !== pending) return
        // Drop the pending claim so subsequent stdout for this turn is discarded; the runtime
        // does NOT kill the child, because the next turn can still reuse it.
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

  /**
   * Cancel the in-flight turn whose id matches. Resolves the originating turn synchronously
   * with a cancellation `Err` and signals the child (SIGINT, with a 2-second SIGTERM
   * watchdog if SIGINT is ignored). Returns `true` iff cancellation took effect.
   */
  cancelInFlight(requestId: string): boolean {
    const pending = this.pending
    if (pending == null || pending.requestId !== requestId) return false
    this.pending = null
    pending.resolve({
      state: 'crash',
      text: '',
      usage: null,
      errorReason: 'cancelled by user',
    })
    this.signalCancel()
    return true
  }

  /** Request graceful shutdown of the child and reject any pending work. Idempotent. */
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
    // Reject the readiness promise if it's still pending — `watcher.close()` sets the watcher's
    // `closed` flag, which suppresses the `onUnexpectedExit` path that would normally reject
    // `readyDeferred`. Without this, a caller `await`ing `this.ready` mid-priming would hang
    // forever. Resolved (or already-rejected) deferreds ignore further calls, so this is safe
    // to call unconditionally.
    if (!this.isReadyResolved) {
      this.readyDeferred.reject(new Error('Claude agent shutting down'))
    }
    void this.watcher.close()
    try {
      fs.rmSync(this.sandboxCwd, { recursive: true, force: true })
    } catch {
      // Already gone or filesystem-level failure; nothing meaningful to do here.
    }
  }

  // -------------- private --------------

  private observeReadyDeferred(deferred: Deferred<void>): void {
    deferred.promise.then(
      () => {
        if (this.readyDeferred === deferred) this.isReadyResolved = true
      },
      () => undefined,
    )
  }

  private swapInReadyDeferred(): void {
    this.isReadyResolved = false
    this.readyDeferred = createDeferred()
    this.observeReadyDeferred(this.readyDeferred)
  }

  private onChildStarted(handle: ChildProcessHandle): void {
    this.stderrTail = ''
    this.pending = null

    const child = handle.child
    if (child.stdout) {
      const rl = readline.createInterface({ input: child.stdout })
      rl.on('line', (line) => this.onStdoutLine(line))
    }
    child.stderr?.on('data', (chunk: Buffer) => this.appendStderr(chunk.toString('utf8')))

    // Capture the deferred for *this* spawn so that a fast crash-and-respawn doesn't cross the
    // wires (a stale prime resolving the next spawn's deferred, or vice versa).
    const deferred = this.readyDeferred
    void this.prime().then(
      () => deferred.resolve(),
      (err) => deferred.reject(err),
    )
  }

  private onUnexpectedExit(reason: string, info: UnexpectedExitInfo): undefined {
    const stderrTail = truncateStderr(this.stderrTail)
    const detail = stderrTail ? `: ${stderrTail}` : ''
    const labelTag = this.config.logLabel ? `:${this.config.logLabel}` : ''
    console.warn(`[AI${labelTag}] claude process crashed (${reason})${detail}`)
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
    // Reject the priming promise so any task awaiting `ready` fails fast. Prefer the original
    // error object (when 'error' fired on the child, e.g. ENOENT) so the consumer can surface
    // the install hint via `.code` instead of parsing the reason string.
    this.readyDeferred.reject(info.exitError ?? new Error(reason))
    this.isReadyResolved = false

    if (info.exceedsCrashLimit) {
      console.warn(
        `[AI${labelTag}] claude crash-loop guard tripped (${MAX_RESPAWNS_IN_WINDOW} crashes within ${RESPAWN_WINDOW_MS}ms); auto-respawn is suspended until the next request.`,
      )
      // Defer to the watcher's default for exceedsCrashLimit (suspend).
      return
    }
    // Prepare a fresh deferred for the upcoming auto-respawn so awaiters that arrive between
    // here and `onChildStarted` see a pending promise instead of the just-rejected one.
    this.swapInReadyDeferred()
  }

  private async prime(): Promise<void> {
    const outcome = await this.runTurn(
      buildPrimingPrompt(this.config.stdlibRoot),
      PRIMING_TIMEOUT_MS,
      null,
      PRIMING_REQUEST_ID,
    )
    if (outcome.state !== 'completed') {
      throw new Error(`priming turn ${outcome.state}: ${outcome.errorReason ?? '(no detail)'}`)
    }
    if (!outcome.text.trim()) {
      throw new Error('priming turn produced no assistant text')
    }
  }

  private onStdoutLine(line: string): void {
    const envelope = parseJsonSafe(line)
    if (envelope == null) return
    const assistant = assistantEnvelopeSchema.safeParse(envelope)
    if (assistant.success) {
      this.captureAssistantContent(assistant.data)
      return
    }
    const result = resultEnvelopeSchema.safeParse(envelope)
    if (result.success) {
      this.resolveTerminal(result.data)
    }
    // Other envelope types (system init, user echo, rate_limit_event, unknown): ignore.
  }

  private captureAssistantContent(env: z.infer<typeof assistantEnvelopeSchema>): void {
    const pending = this.pending
    if (!pending) return
    pending.hopCount += 1
    pending.lastHopUsage = env.message.usage ?? null
    const requestId = pending.requestId
    for (const block of env.message.content) {
      if (block.type === 'text' && block.text != null) {
        pending.textChunks.push(block.text)
        if (block.text.trim().length > 0) {
          this.emitProgress({ requestId, kind: 'text', text: block.text })
        }
      } else if (block.type === 'tool_use' && block.name != null) {
        // Emit from here (not `aiMcpServer.dispatchToRenderer`) so built-in `Read`/`Glob`/`Grep`
        // — which the CLI runs itself and never sends to our MCP server — are also captured.
        this.emitProgress({
          requestId,
          kind: 'tool',
          toolName: block.name,
          input: block.input ?? null,
        })
      }
    }
  }

  /**
   * Drops silently when the pending slot has rotated or the originating sender is gone. The
   * pending-slot gate is what distinguishes this from `claudeAgent.ts`'s top-level
   * `emitProgressTo`, which is used for `queued` events that must fire *before* any pending
   * slot exists.
   */
  private emitProgress(event: AiProgressEvent): void {
    const pending = this.pending
    if (pending == null || pending.requestId !== event.requestId) return
    const sender = pending.sender
    if (sender == null || sender.isDestroyed()) return
    try {
      sender.send(Channel.aiProgress, event)
    } catch {
      // Renderer destroyed mid-emit; nothing to do — the next progress check will short-circuit.
    }
  }

  private resolveTerminal(env: z.infer<typeof resultEnvelopeSchema>): void {
    if (!this.pending) return
    const pending = this.pending
    this.pending = null
    const text =
      env.result != null && env.result.length > 0 ? env.result : pending.textChunks.join('')
    pending.resolve({
      state: 'completed',
      text,
      usage: env.usage ?? null,
    })
  }

  private appendStderr(chunk: string): void {
    this.stderrTail = (this.stderrTail + chunk).slice(-STDERR_TAIL_CHARS)
  }

  /** SIGINT then SIGTERM-after-2s; best-effort, since the cancellation Err already landed. */
  private signalCancel(): void {
    const handle = this.watcher.current
    if (handle == null || !handle.alive) return
    try {
      handle.child.kill('SIGINT')
    } catch {
      // ignore — already exiting
    }
    setTimeout(() => {
      const stillAliveHandle = this.watcher.current
      if (stillAliveHandle == null || !stillAliveHandle.alive) return
      if (stillAliveHandle !== handle) return
      try {
        stillAliveHandle.child.kill('SIGTERM')
      } catch {
        // ignore
      }
    }, CANCEL_SIGINT_TO_SIGTERM_MS)
  }
}

/**
 * Format a `ready`-rejection error for the renderer-facing reply. ENOENT (`claude` not on
 * PATH) gets a self-teaching hint; everything else keeps the underlying message.
 */
export function formatNotReadyError(err: unknown): string {
  const errno = err as NodeJS.ErrnoException | null
  if (errno?.code === 'ENOENT') {
    return `'${CLAUDE_EXECUTABLE}' executable not found on PATH — install Claude Code to use the AI node feature`
  }
  const message = errno?.message ?? String(err ?? 'unknown error')
  return `Claude agent is not ready: ${message}`
}

/** Internal name of the `claude` executable; surfaced for log-line assertions in tests. */
export const CLAUDE_EXECUTABLE_NAME = CLAUDE_EXECUTABLE
