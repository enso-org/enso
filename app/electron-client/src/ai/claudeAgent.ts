/**
 * @file Drives the long-lived `claude` CLI subprocess(es) that generate User Defined Component
 * bodies from natural-language prompts. Authentication rides on whatever the CLI is already
 * configured with — the main process never reads the API key directly. Per-child plumbing
 * (spawn args, priming, stream-json parsing, signal-cancel, crash bookkeeping) lives in
 * `claudeAgentChild.ts`; see {@link ClaudeAgentSession} for orchestration details and
 * `electron-client/CLAUDE.md` for the wire-format and lifecycle background.
 */
import { ipcMain, type WebContents } from 'electron'
import {
  aiComponentResponseSchema,
  type AiCancelRequest,
  type AiComponentIpcReply,
  type AiComponentRequest,
  type AiProgressEvent,
  type RequestUsage,
} from 'enso-common/src/ai'
import { AsyncQueue } from 'enso-common/src/utilities/async'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { Channel } from '../ipc.js'
import { startAiMcpServer, type AiMcpServer } from './aiMcpServer.js'
import {
  ChildAgent,
  formatNotReadyError,
  type ActiveRequest,
  type ChildAgentConfig,
  type RawTokenUsage,
  type TurnOutcome,
} from './claudeAgentChild.js'
import { buildUserPrompt } from './prompts.js'

export type { ActiveRequest } from './claudeAgentChild.js'

// Tool round-trips (filesystem reads, MCP `evaluateExpression` calls) eat the budget fast — a
// single turn can do half a dozen sub-second LS queries on top of the model's own output. The
// pre-tools value was 120s; tripled with headroom for the worst-case fan-out.
const REQUEST_TIMEOUT_MS = 360_000

/**
 * Default soft threshold (tokens). Above this, a fresh `claude` child starts priming in the
 * background; the existing child keeps serving turns until the new one is ready. Override
 * with `ENSO_AI_SOFT_CONTEXT_THRESHOLD`.
 */
const SOFT_CONTEXT_THRESHOLD_TOKENS = 300_000

/**
 * Default hard threshold (tokens). Above this, new turns block on the warming child becoming
 * ready — the existing child no longer serves new turns. Override with
 * `ENSO_AI_HARD_CONTEXT_THRESHOLD`.
 */
const HARD_CONTEXT_THRESHOLD_TOKENS = 400_000

/** Configuration for the long-lived `claude` session, passed at IPC init time. */
export interface ClaudeSessionConfig extends ChildAgentConfig {
  /** Soft rotation threshold in context tokens; falls back to env var, then default. */
  readonly softThreshold?: number
  /** Hard rotation threshold in context tokens; falls back to env var, then default. */
  readonly hardThreshold?: number
}

interface ResolvedThresholds {
  readonly soft: number
  readonly hard: number
}

type ThresholdEnvVar = 'ENSO_AI_SOFT_CONTEXT_THRESHOLD' | 'ENSO_AI_HARD_CONTEXT_THRESHOLD'

function readEnvThreshold(name: ThresholdEnvVar, fallback: number): number {
  const raw = process.env[name]
  if (raw == null || raw === '') return fallback
  const n = Number.parseInt(raw, 10)
  if (Number.isFinite(n) && n > 0) return n
  console.warn(`[AI] invalid ${name}=${raw}; falling back to default ${fallback}.`)
  return fallback
}

function resolveThresholds(config: ClaudeSessionConfig): ResolvedThresholds {
  const soft =
    config.softThreshold ??
    readEnvThreshold('ENSO_AI_SOFT_CONTEXT_THRESHOLD', SOFT_CONTEXT_THRESHOLD_TOKENS)
  const hard =
    config.hardThreshold ??
    readEnvThreshold('ENSO_AI_HARD_CONTEXT_THRESHOLD', HARD_CONTEXT_THRESHOLD_TOKENS)
  if (hard < soft) {
    console.warn(`[AI] hard threshold ${hard} < soft threshold ${soft}; reverting to defaults.`)
    return { soft: SOFT_CONTEXT_THRESHOLD_TOKENS, hard: HARD_CONTEXT_THRESHOLD_TOKENS }
  }
  return { soft, hard }
}

/**
 * Read `ENSO_AI_CLAUDE_EXTRA_ARGS` and split on whitespace; empty/unset → `undefined`.
 * No shell-style quoting — values containing whitespace aren't expressible.
 */
function readExtraArgsEnv(): readonly string[] | undefined {
  const raw = process.env.ENSO_AI_CLAUDE_EXTRA_ARGS
  if (raw == null) return undefined
  const tokens = raw.split(/\s+/).filter((t) => t.length > 0)
  return tokens.length > 0 ? tokens : undefined
}

function parseJsonSafe(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

/**
 * Try strict JSON.parse first; on failure, fall back to extracting the last balanced top-level
 * `{…}` object in the text. The fallback covers the case where the model leaks narration prose
 * into its closing turn instead of emitting JSON-only.
 */
export function extractJsonObject(text: string): unknown {
  const direct = parseJsonSafe(text)
  if (direct != null && typeof direct === 'object') return direct
  let depth = 0
  let start = -1
  let inString = false
  let escape = false
  let bestStart = -1
  let bestEnd = -1
  for (let i = 0; i < text.length; i++) {
    const ch = text[i]
    if (escape) {
      escape = false
      continue
    }
    if (ch === '\\') {
      escape = true
      continue
    }
    if (ch === '"') {
      inString = !inString
      continue
    }
    if (inString) continue
    if (ch === '{') {
      if (depth === 0) start = i
      depth++
    } else if (ch === '}') {
      depth--
      if (depth === 0 && start !== -1) {
        bestStart = start
        bestEnd = i
        start = -1
      }
    }
  }
  if (bestStart < 0 || bestEnd < 0) return null
  return parseJsonSafe(text.slice(bestStart, bestEnd + 1))
}

/**
 * Send a progress event to a specific renderer without going through the per-turn `pending` slot.
 * Used for `queued` acknowledgments emitted before the turn begins (and thus before any
 * `pending` is set on a child) — once `pending` is set, {@link ChildAgent.emitProgress} is the
 * canonical emitter.
 */
function emitProgressTo(sender: WebContents, event: AiProgressEvent): void {
  if (sender.isDestroyed()) return
  try {
    sender.send(Channel.aiProgress, event)
  } catch {
    // Renderer destroyed mid-emit; the next send will short-circuit on isDestroyed().
  }
}

/** State of an in-progress context-rotation. See {@link ClaudeAgentSession} for semantics. */
type SwapMode = 'none' | 'soft' | 'hard'

// ============================
// === ClaudeAgentSession   ===
// ============================

/**
 * Orchestrates one (or, mid-rotation, two) {@link ChildAgent} instances — one is the active
 * "primary" serving turns, and a second "warming" child may be priming in the background as
 * part of the context-rotation policy below. Turns are serialized across both children
 * through a single session-level FIFO queue, and the agent's reply is validated against the
 * component schema.
 *
 * **Context-rotation policy.** Every `claude` turn appends to its child's CLI-side
 * conversation history, so `RequestUsage.contextTokens` grows approximately monotonically
 * across the session. To avoid hitting the model's context window, the session watches the
 * post-turn `contextTokens` value:
 *
 * - Above the **soft** threshold (default 300K, env `ENSO_AI_SOFT_CONTEXT_THRESHOLD`), a
 *   fresh "warming" child is spawned in the background. The existing primary keeps serving
 *   new turns until the warming child is primed; the next queue task after that promotes it.
 * - Above the **hard** threshold (default 400K, env `ENSO_AI_HARD_CONTEXT_THRESHOLD`), the
 *   warming child is spawned (if not already), AND new queue tasks block on its readiness
 *   — the primary no longer serves new turns. If priming fails, the awaiting task falls
 *   back to primary; the next over-threshold turn re-arms the swap.
 *
 * **Already-running turns are never cancelled** by a swap. The queue's serial execution
 * guarantees that the threshold check happens *after* the current turn settles, so any
 * in-flight HTTPS stream to Anthropic completes normally.
 */
export class ClaudeAgentSession {
  private readonly config: ClaudeSessionConfig
  private readonly thresholds: ResolvedThresholds
  private readonly extraArgs: readonly string[] | undefined
  private primary: ChildAgent
  private warming: ChildAgent | null = null
  private swapMode: SwapMode = 'none'
  private readonly queue = new AsyncQueue<void>(Promise.resolve())
  private disposed = false
  /** Ids cancelled while still queued behind another turn; consumed by the queue task on entry. */
  private readonly cancelled = new Set<string>()
  /**
   * Ids currently in the queue or running. Populated synchronously in {@link runRequest} before
   * the task is pushed, cleared in the task's `finally` once it settles.
   */
  private readonly liveRequests = new Set<string>()
  /**
   * Set inside {@link promotePrimedWarming} the moment a warming child is promoted, and cleared
   * by the first {@link replyFromTurn} that emits non-null usage — i.e. the first turn whose
   * outcome actually reaches the renderer. The cleared bit flips `RequestUsage.freshAgent` to
   * `true` for that single reply, then back to `false`. Cancellations/disposed-session
   * short-circuits and `ready`-rejections do NOT consume the flag, because their replies carry
   * no usage and the conversation history is still fresh for whatever runs next.
   */
  private freshAgentPending = false

  /** Resolve thresholds and spawn the primary child eagerly. */
  constructor(config: ClaudeSessionConfig) {
    this.config = config
    this.thresholds = resolveThresholds(config)
    this.extraArgs = config.extraArgs ?? readExtraArgsEnv()
    this.primary = new ChildAgent({ ...this.childConfig(), logLabel: 'primary' })
  }

  /** Resolves once the current primary child has spawned and accepted the priming turn. */
  get ready(): Promise<void> {
    return this.primary.ready
  }

  /**
   * Resolves to `true` once the primary `claude` child has spawned without a synchronous
   * ENOENT-style failure, `false` if the spawn failed. Decoupled from priming — the CLI counts
   * as "available" as soon as the process is alive.
   */
  get isAvailable(): Promise<boolean> {
    return this.primary.firstSpawnSettled
  }

  /**
   * The renderer + request id driving the in-flight turn, or `null` between turns / when the
   * renderer was destroyed. Only one of (primary, warming) ever has a non-null `activeRequest`
   * at a time, since the AsyncQueue serializes turn execution across both.
   */
  get activeRequest(): ActiveRequest | null {
    return this.primary.activeRequest ?? this.warming?.activeRequest ?? null
  }

  /** Run an AI component request through the long-lived session. */
  runRequest(request: AiComponentRequest, sender: WebContents): Promise<AiComponentIpcReply> {
    return new Promise<AiComponentIpcReply>((resolveOuter) => {
      if (sender.isDestroyed()) {
        resolveOuter({
          result: Err('Renderer was destroyed before the AI request could be handled'),
          usage: null,
        })
        return
      }
      // Tracked synchronously here so a cancel arriving between this point and the task's
      // entry-check is recognized as live and gets filed into `cancelled` rather than dropped.
      this.liveRequests.add(request.requestId)
      this.queue.pushTask(async () => {
        try {
          // Cancellation that arrived while the request was queued behind another turn: the
          // pending slot didn't match this `requestId` at cancel time, so the cancel was filed in
          // the `cancelled` set. Consume it here before paying the cost of a real turn.
          if (this.cancelled.delete(request.requestId)) {
            resolveOuter({ result: Err('Cancelled by user'), usage: null })
            return
          }
          // Acknowledge IPC receipt now that we know we're going to actually run the turn. For
          // an uncontended request `started` follows microseconds later, but multi-window or
          // post-priming contention can keep us here long enough for the renderer to want a
          // "we got it" signal.
          emitProgressTo(sender, { requestId: request.requestId, kind: 'queued' })
          if (this.disposed) {
            resolveOuter({ result: Err('Claude agent has been shut down'), usage: null })
            return
          }
          // Check whether the warming child has finished priming and is ready to take over.
          this.promotePrimedWarming()
          // Hard mode: block this turn on warming readiness. If priming fails, fall back to
          // primary (still alive), reset the swap state, and let the next over-threshold turn
          // re-arm the rotation.
          if (this.swapMode === 'hard' && this.warming != null) {
            const warming = this.warming
            try {
              await warming.ready
              this.promotePrimedWarming()
            } catch (err) {
              console.warn(
                `[AI] warming child failed to prime; falling back to primary: ${(err as Error)?.message ?? String(err)}`,
              )
              // `this.warming === warming` guards against a concurrent `shutdown()` having
              // already nulled the slot during the await — without it, we'd reset `swapMode`
              // and clobber state owned by the (already-disposed) shutdown path.
              if (this.warming === warming) {
                warming.shutdown()
                this.warming = null
                this.swapMode = 'none'
              }
            }
          }
          if (this.primary.respawnSuspended && !this.primary.alive) {
            // The watcher's recent-exits buffer is preserved across respawn(), so a quick
            // re-crash trips the guard again and we don't loop indefinitely.
            await this.primary.respawn()
          }
          try {
            await this.primary.ready
          } catch (err) {
            resolveOuter({ result: Err(formatNotReadyError(err)), usage: null })
            return
          }
          // Re-check the deferred-cancel set: a `cancelTurn` could arrive during any of the awaits
          // above.
          if (this.cancelled.delete(request.requestId)) {
            resolveOuter({ result: Err('Cancelled by user'), usage: null })
            return
          }
          const turn = await this.primary.runTurn(
            buildUserPrompt(request),
            REQUEST_TIMEOUT_MS,
            sender,
            request.requestId,
          )
          const reply = this.replyFromTurn(turn)
          resolveOuter(reply)
          // Post-turn: evaluate context pressure and (re)arm a swap if the threshold is crossed.
          // Done after `resolveOuter` so the IPC reply isn't blocked on swap bookkeeping.
          if (reply.usage != null) {
            this.evaluateThresholds(reply.usage.contextTokens)
          }
        } finally {
          // Drop both bookkeeping entries no matter how the task settled (success, early-return,
          // throw). This is what bounds {@link cancelled} — any id added by a `cancelTurn` whose
          // task body has already passed the entry-check gets cleaned up here.
          this.cancelled.delete(request.requestId)
          this.liveRequests.delete(request.requestId)
        }
      })
    })
  }

  /**
   * Cancel a previously-dispatched request. For the in-flight slot the originating turn resolves
   * synchronously with a cancellation `Err` and an in-band `control_request`/`interrupt`
   * envelope is written to the child's stdin — the warm conversation context is preserved when
   * the CLI honors it (falling back to a SIGTERM→SIGKILL escalation only if it doesn't). Queued
   * requests file the id in {@link cancelled} for the queue task to short-circuit. Idempotent.
   */
  cancelTurn(requestId: string): void {
    if (this.disposed) return
    if (this.primary.cancelInFlight(requestId)) return
    if (this.warming?.cancelInFlight(requestId)) return
    // Only file a deferred cancel when the id is one we're tracking — cancels for ids that have
    // already settled, or that were never issued, would otherwise accumulate in the set forever.
    if (this.liveRequests.has(requestId)) {
      this.cancelled.add(requestId)
    }
  }

  /** Request graceful shutdown of all children and reject any pending work. Idempotent. */
  shutdown(): void {
    if (this.disposed) return
    this.disposed = true
    this.primary.shutdown()
    if (this.warming != null) {
      this.warming.shutdown()
      this.warming = null
    }
  }

  // -------------- private --------------

  private childConfig(): ChildAgentConfig {
    return {
      stdlibRoot: this.config.stdlibRoot,
      mcpConfigPath: this.config.mcpConfigPath,
      extraArgs: this.extraArgs,
    }
  }

  /**
   * Promote the warming child if it has primed (or discard it if its watcher is permanently
   * suspended). Called at the start of every queue task and immediately after the hard-mode
   * `await warming.ready`.
   */
  private promotePrimedWarming(): void {
    const warming = this.warming
    if (warming == null) return
    if (warming.isReady) {
      const oldPrimary = this.primary
      this.primary = warming
      this.warming = null
      this.swapMode = 'none'
      this.freshAgentPending = true
      oldPrimary.shutdown()
      console.info('[AI] rotated to fresh claude agent.')
      return
    }
    if (warming.respawnSuspended && !warming.alive) {
      // Crash-loop guard tripped before warming could prime; discard so the next over-threshold
      // turn re-arms a fresh warming attempt.
      console.warn('[AI] warming child stuck in crash-loop before priming; discarding.')
      warming.shutdown()
      this.warming = null
      this.swapMode = 'none'
    }
  }

  private evaluateThresholds(contextTokens: number): void {
    if (this.warming != null) {
      // Already warming. Upgrade to hard if the latest turn pushed us past it.
      // The reverse — tearing the warming child down when `contextTokens` falls back below
      // soft — is deliberately not implemented.
      if (contextTokens > this.thresholds.hard && this.swapMode !== 'hard') {
        console.info(
          `[AI] context ${contextTokens} > hard ${this.thresholds.hard}; upgrading swap to hard.`,
        )
        this.swapMode = 'hard'
      }
      return
    }
    if (contextTokens > this.thresholds.hard) {
      this.spawnWarming('hard', contextTokens)
    } else if (contextTokens > this.thresholds.soft) {
      this.spawnWarming('soft', contextTokens)
    }
  }

  private spawnWarming(mode: 'soft' | 'hard', contextTokens: number): void {
    if (this.warming != null) return
    const threshold = this.thresholds[mode]
    console.info(
      `[AI] context ${contextTokens} > ${mode} ${threshold}; spawning warming claude child.`,
    )
    this.warming = new ChildAgent({ ...this.childConfig(), logLabel: 'warming' })
    this.swapMode = mode
    // Attach a no-op rejection handler so a priming failure with no awaiter (soft mode) doesn't
    // surface as an unhandled rejection. The hard-mode awaiter attaches its own handler.
    this.warming.ready.catch(() => undefined)
  }

  private replyFromTurn(turn: TurnOutcome): AiComponentIpcReply {
    // Only consume the rotation flag when the reply will actually carry a `RequestUsage` to
    // the renderer. A crashed turn (no `turn.usage`) yields `usage: null` from
    // {@link snapshotUsage}, so its `freshAgent` would be lost; keeping `freshAgentPending`
    // armed in that case lets the first turn that *does* carry usage signal the rotation.
    const willEmitUsage = turn.usage != null
    const freshAgent = willEmitUsage && this.freshAgentPending
    if (willEmitUsage) this.freshAgentPending = false
    const usage = snapshotUsage(
      turn.usage,
      turn.lastHopUsage,
      turn.hopCount,
      turn.durationMs,
      freshAgent,
    )
    if (turn.state !== 'completed') {
      const reason = turn.errorReason ?? 'claude turn failed'
      return { result: Err(`Claude agent: ${reason}`), usage }
    }
    if (!turn.text.trim()) {
      return { result: Err('Claude agent returned an empty reply'), usage }
    }
    const parsedJson = extractJsonObject(turn.text)
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
}

/**
 * Combine a turn's cost-side `usage` (from the `result` envelope) with the final assistant
 * envelope's per-hop `usage` to produce the renderer-facing `RequestUsage`. See
 * `app/common/src/ai.ts:RequestUsage` for the field semantics — `contextTokens` prefers the
 * last hop and falls back to the cost-side sum (overstates on multi-hop turns; the consumer
 * surfaces the source via `contextFromLastHop`).
 */
function snapshotUsage(
  raw: RawTokenUsage | null,
  lastHop: RawTokenUsage | null,
  hopCount: number,
  durationMs: number,
  freshAgent: boolean,
): RequestUsage | null {
  if (!raw) return null
  const contextFromLastHop = lastHop != null
  const contextSource = lastHop ?? raw
  const contextInput = contextSource.input_tokens ?? 0
  const contextCacheRead = contextSource.cache_read_input_tokens ?? 0
  const contextCacheCreation = contextSource.cache_creation_input_tokens ?? 0
  return {
    inputTokens: raw.input_tokens ?? 0,
    outputTokens: raw.output_tokens ?? 0,
    cacheReadTokens: raw.cache_read_input_tokens ?? 0,
    cacheCreationTokens: raw.cache_creation_input_tokens ?? 0,
    contextTokens: contextInput + contextCacheRead + contextCacheCreation,
    contextFromLastHop,
    hopCount,
    durationMs,
    freshAgent,
  }
}

// ===================
// === IPC binding ===
// ===================

let session: ClaudeAgentSession | null = null
let mcpServer: AiMcpServer | null = null

/**
 * Start the in-process MCP server that exposes `evaluateExpression` to the agent. Returns the
 * config file path to pass into {@link initClaudeAgentIpc}, or `undefined` if startup failed.
 */
export async function initAiMcpServer(): Promise<string | undefined> {
  try {
    const started = await startAiMcpServer(() => session?.activeRequest ?? null)
    mcpServer = started.server
    console.info(`[AI] MCP server config at ${started.mcpConfigPath}`)
    return started.mcpConfigPath
  } catch (err) {
    console.warn(
      `[AI] failed to start in-process MCP server; the agent will run without the evaluateExpression tool:`,
      err,
    )
    return undefined
  }
}

/**
 * Spawn the long-lived agent session and register the {@link Channel.generateAiComponent} IPC
 * handler. Pass `mcpConfigPath` from {@link initAiMcpServer} (or `undefined` to disable MCP).
 *
 * Honors `ENSO_AI_DISABLED=1`: when set, the session is NOT spawned, `aiIsAvailable` reports
 * `false`, and any stray call to `generateAiComponent` returns a structured error.
 */
export function initClaudeAgentIpc(config: ClaudeSessionConfig): void {
  if (process.env.ENSO_AI_DISABLED === '1') {
    console.info(`[AI] ENSO_AI_DISABLED=1; skipping 'claude' session startup.`)
    ipcMain.handle(Channel.aiIsAvailable, async () => false)
    ipcMain.handle(
      Channel.generateAiComponent,
      async (): Promise<AiComponentIpcReply> => ({
        result: Err('AI is disabled by ENSO_AI_DISABLED'),
        usage: null,
      }),
    )
    ipcMain.on(Channel.cancelAiComponent, () => {
      // No-op: there's no session to cancel against.
    })
    return
  }
  if (config.stdlibRoot == null) {
    console.warn(
      `[AI] could not locate the bundled engine's lib/Standard directory; the agent will run without stdlib filesystem access.`,
    )
  } else {
    console.info(`[AI] stdlib filesystem access at ${config.stdlibRoot}`)
  }
  if (session == null) session = new ClaudeAgentSession(config)
  const currentSession = session
  // Surface a missing-CLI hint via the first `ready` rejection — saves spawning a `--version` probe.
  void currentSession.ready.catch((err) => {
    const errno = err as NodeJS.ErrnoException | null
    if (errno?.code === 'ENOENT') {
      console.warn(
        `[AI] 'claude' not found on PATH; AI node generation will fail until Claude Code is installed.`,
      )
    } else {
      console.warn(`[AI] failed to start 'claude' session: ${errno?.message ?? String(err)}`)
    }
  })
  ipcMain.handle(Channel.aiIsAvailable, async () => currentSession.isAvailable)
  ipcMain.handle(
    Channel.generateAiComponent,
    async (event, request: AiComponentRequest): Promise<AiComponentIpcReply> =>
      currentSession.runRequest(request, event.sender),
  )
  ipcMain.on(Channel.cancelAiComponent, (_event, payload: AiCancelRequest) => {
    currentSession.cancelTurn(payload.requestId)
  })
}

/** Tear down the long-lived session and MCP server. Wired to `app.on('before-quit', ...)`. */
export function shutdownClaudeAgent(): void {
  session?.shutdown()
  session = null
  if (mcpServer != null) {
    void mcpServer.shutdown()
    mcpServer = null
  }
}
