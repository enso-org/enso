/**
 * @file Drives a long-lived `claude` CLI subprocess that generates User Defined Component bodies
 * from natural-language prompts. Authentication rides on whatever the CLI is already configured
 * with — the main process never reads the API key directly.
 *
 * Lifecycle, queueing, crash handling, and the stream-json wire format are documented in
 * `electron-client/CLAUDE.md`.
 */
import spawn from 'cross-spawn'
import { ipcMain, type WebContents } from 'electron'
import {
  aiComponentResponseSchema,
  type AiCancelRequest,
  type AiComponentIpcReply,
  type AiComponentRequest,
  type AiProgressEvent,
  type RequestUsage,
} from 'enso-common/src/ai'
import { AsyncQueue, createDeferred, type Deferred } from 'enso-common/src/utilities/async'
import {
  ChildProcessHandle,
  WatchedChildProcess,
  type UnexpectedExitInfo,
} from 'enso-common/src/utilities/childProcess'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import readline from 'node:readline'
import { z } from 'zod'
import { startAiMcpServer, type AiMcpServer } from './aiMcpServer.js'
import { Channel } from './ipc.js'

const CLAUDE_EXECUTABLE = 'claude'
// Tool round-trips (filesystem reads, MCP `evaluateExpression` calls) eat the budget fast — a
// single turn can do half a dozen sub-second LS queries on top of the model's own output. The
// pre-tools value was 120s; tripled with headroom for the worst-case fan-out.
const REQUEST_TIMEOUT_MS = 360_000
// Priming reads three CLAUDE.md files (top-level, Base, Table) plus the entire `Standard.Image`
// source as a syntax demo (when stdlib is available); ~12 tool calls before replying. 180s leaves
// comfortable headroom while staying well under REQUEST_TIMEOUT_MS. The fallback path (no stdlib)
// only does the trivial "say READY" turn and completes in well under a second, so the higher cap
// costs nothing on that path.
const PRIMING_TIMEOUT_MS = 180_000
const STDERR_TAIL_CHARS = 2_000
const RESPAWN_WINDOW_MS = 30_000
const MAX_RESPAWNS_IN_WINDOW = 3

function buildPrimingPrompt(config: ClaudeSessionConfig): string {
  if (config.stdlibRoot == null) {
    return 'Acknowledge readiness with the single word READY. This is a session warm-up; do not return JSON.'
  }
  return `This is a session warm-up. Before any user request arrives, study the bundled Enso standard library so you can produce correct code for it.

Step 1. Read these three files entirely, do not skip paragraphs:
- \`${config.stdlibRoot}/CLAUDE.md\` — universal stdlib conventions, library index, and cross-library pitfalls. Applies to every library; the rules here are not repeated in per-library files.
- \`${config.stdlibRoot}/Base/0.0.0-dev/CLAUDE.md\` — foundational types and operations. Every other library imports from \`Base\`.
- \`${config.stdlibRoot}/Table/0.0.0-dev/CLAUDE.md\` — columnar in-memory and database-backed table operations. The most-used non-\`Base\` library.

Other libraries each have a similar \`<Name>/0.0.0-dev/CLAUDE.md\`. **Do not Read those now.** When a later user request needs a library you have not yet loaded, Read that library's \`CLAUDE.md\` then; the content stays in context for the rest of the session.

Step 2. Glob \`Image/0.0.0-dev/src/**/*.enso\` under \`${config.stdlibRoot}\` and Read every match. The Image library is small (~9 files) and shows idiomatic Enso syntax in a real, complete library — types, methods, conversions, doc blocks.

Once you have finished both steps, reply with a single word: READY. Do not return JSON for this warm-up.`
}

/** Synthetic request id used for the priming turn. */
const PRIMING_REQUEST_ID = 'priming'
/** SIGINT (graceful) to SIGTERM (force respawn) escalation window. */
const CANCEL_SIGINT_TO_SIGTERM_MS = 2_000

/** Configuration for the long-lived `claude` session. */
export interface ClaudeSessionConfig {
  /**
   * Bundled engine's `lib/Standard` directory, exposed to the agent via `--add-dir` plus the
   * `Read`/`Glob`/`Grep` tools. `undefined` disables stdlib filesystem access.
   */
  readonly stdlibRoot: string | undefined
  /**
   * Config file produced by {@link AiMcpServer.start}, exposing the `evaluateExpression` tool.
   * `undefined` runs the agent without MCP (used by headless tests).
   */
  readonly mcpConfigPath: string | undefined
}

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
- Constructors: a type holds its constructors. Build a value with \`Type.Constructor args\` (e.g. \`Constant_Column.Value "x"\`) — a bare type name isn't callable unless it has a single constructor sharing its name. The shorthand \`..Constructor args\` (autoscope) only resolves when the surrounding parameter's expected type is a *single* type with that constructor; for parameters typed as a union (\`A | B | C\`) use the qualified \`Type.Constructor\` form.
- Conversions: many wide-input methods accept several source types via \`Target.from (that:Source)\` conversion methods on the target — when the parameter is a union, passing a value of any accepted source type lets the engine coerce it, no wrapper needed.
Common stdlib entry points (Standard.Base / Standard.Table):
- \`Vector.new count fn\`, \`Vector.filter\`, \`Vector.map\`, \`Vector.reduce\`.
- \`Table.filter\`, \`Table.select_columns\`, \`Table.sort\`, \`Table.aggregate\`.
- \`Text.contains\`, \`Text.starts_with\`, \`Text.split\`.
- \`Data.read path\`, \`Data.write path value\`.`

function buildSystemPrompt(config: ClaudeSessionConfig): string {
  const toolLines: string[] = []
  if (config.stdlibRoot != null) {
    toolLines.push(
      `- \`Read\`, \`Glob\`, and \`Grep\` against the Enso standard library at \`${config.stdlibRoot}\`. Prefer reading the actual \`.enso\` source files when in doubt about a function's exact name, signature, or available overloads — your built-in cheat sheet is incomplete. Stay inside that directory; do not attempt to read anything else.`,
      `- The stdlib has a top-level \`${config.stdlibRoot}/CLAUDE.md\` (universal conventions, library index, cross-library pitfalls) plus a per-library \`${config.stdlibRoot}/<Name>/0.0.0-dev/CLAUDE.md\` (each library's public API, common usage, pitfalls). You read the top-level file plus \`Base\`'s and \`Table\`'s at session start. **Before using any other library, Read its \`CLAUDE.md\` once first** — it is more compact than the source and quicker than \`Glob\`/\`Grep\` for orientation. The result stays in context for the rest of the session.`,
      `- When unsure about a method's arguments, return type, or usage examples, \`Read\` the source file containing it and consult the \`## \` doc block immediately preceding the definition. Doc blocks list arguments under \`## Arguments\` and runnable examples under \`## Examples\`; they are the authoritative reference, more reliable than guessing from the name alone.`,
      `- Do not call into anything under an \`Internal/\` module path, or any entity whose \`## ---\` metadata block contains \`private: true\`, or any module whose first non-blank source line is \`private\`. These are unstable helpers without API guarantees. Prefer the public API re-exported from each library's \`Main.enso\`.`,
    )
  }
  if (config.mcpConfigPath != null) {
    toolLines.push(
      '- `evaluateExpression(expression)` — evaluate a plain Enso expression in the same scope your generated `body` would run in. Every in-scope binding listed below is referenceable by name. **The expression must evaluate to Text** (the transport sends raw bytes back as text); pick the encoding yourself with `.to_text`, `.to_display_text`, or `.to_json` depending on what is most useful. For non-Text producers wrap the call: `<binding>.column_names.to_json` (JSON array of column names), `(<binding>.first.to_text).take 200` (preview a value), `(<binding>.row_count).to_text` (a single number), `(cards.join leader_order on=["Set"]).column_names.to_json` (JSON array, schema check). Expressions that may produce a DataflowError need an explicit catch: `((<expr>).catch_primitive (e -> e.to_display_text))` — otherwise the call comes back with the dataflow error wrapped in an actionable hint, not the value you wanted. Each call is a real LS round-trip — pick what you need, don\'t fan out.',
    )
  }
  const toolsSection =
    toolLines.length > 0 ? `\n\nTools you have available:\n${toolLines.join('\n')}` : ''
  return `\
You generate a top-level User Defined Component in Enso — a function definition plus the call that places it inside an existing method on the user's graph.

${ENSO_CHEAT_SHEET}${toolsSection}

You will receive:
- The Enso method the call site lives in (its name and full source).
- The list of \`import\` / \`from … import …\` statements already at the top of the module. Names brought in by these imports are resolvable unqualified; everything else needs a fully qualified name (you cannot add new imports — your only output is the function definition + call). When the surrounding call site is unambiguous about the expected type, the \`..Constructor\` autoscope shorthand avoids long qualified names — see the cheat sheet above for when this applies vs. when you must use \`Type.Constructor\` or rely on a \`Target.from (that:Source)\` conversion.
- Optionally a source binding the user dropped into the AI prompt (identifier and Enso type, when known) — when present, this is the value they want to operate on.
- Other identifiers already in scope in that method, with their Enso types when known. You may reference any of them.
- A natural-language description of what the new component should do.

**Live progress narration (REQUIRED whenever you use any tool).** The user sees these notes as the placeholder node's status text. Every time you start a new logical step that uses tools, emit ONE short text block (≤8 words, present continuous, no code or paths) describing that step in plain English — e.g. "Checking Pokemon Cards columns", "Looking up Table.join signature", "Reading Standard.Table source", "Drafting filter step". One narration covers all the tool calls within that step; you don't need to restate it per tool call. The narration text block must come before the tool_use blocks for that step. If you skip the narration the user just sees "Thinking…" and feels stuck; treat it as part of the contract, not optional. Code, expression text, and file paths must NOT appear in the narration — those are logged separately.

Your closing assistant turn (after the last tool round, or right away if you don't use tools) must be the JSON object described below and nothing else (no narration, no prose, no code fences, no leading or trailing whitespace):
- \`functionName\`: snake_case identifier for the new top-level function. It must not collide with an identifier already used in the surrounding method or with a name visible in the supplied method source. Pick something descriptive of what the function does.
- \`argumentNames\`: parameter names in the function signature, in declaration order. Pick names that describe each parameter's role inside the function — they do *not* have to match any in-scope identifier and they are the names you reference inside \`body\`. Only declare parameters that \`body\` actually uses.
- \`body\`: the function body, as a string. Every line belongs to the body; no leading or trailing blank lines. Reference the parameters by the names you listed in \`argumentNames\`. The final line must be a single identifier — the binding that holds the result. Do not include the function signature, the \`=\` sign, or any module wrapper.
- \`callArguments\`: Enso expressions passed at the call site, one per parameter and in the same order as \`argumentNames\`. Each entry is usually just an in-scope identifier (the source binding or one of the other in-scope bindings), but any single Enso expression is accepted. The renderer wraps them as \`Main.<functionName> <callArguments[0]> <callArguments[1]> ...\`. When a source binding is provided, pass it as a call argument if the function operates on it; pass other in-scope identifiers only when the function uses them. The function may also take no parameters at all if it doesn't depend on anything in scope.

Rules:
- Do not chain method calls on a single line — every line in \`body\` should be at most one outer call so each step shows up as its own graph node. Avoid \`x.foo y . bar z\` and \`x.foo.bar\`; bind the intermediate result to a name and call \`.bar\` on the next line. **Calls inside arguments are fine** — write small constructors and helpers directly as arguments rather than naming intermediates for them, e.g. \`table.filter "age" (..Greater 18)\` is one call, not two (the inner \`..Greater 18\` is an autoscoped \`Filter_Condition\` argument, not a chain).
- The final line of \`body\` must be a single identifier — assign expressions to a name first and reference that name.
- \`argumentNames\` and \`callArguments\` must have the same length.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.

If the user message is a session warm-up and the request is not for a component, reply briefly in plain text. Otherwise, your closing assistant turn must be the JSON object described above and nothing else.`
}

// =================
// === Prompt IO ===
// =================

function formatBinding(identifier: string, typeName: string | undefined): string {
  return typeName ? `- ${identifier} : ${typeName}` : `- ${identifier} : (type unknown)`
}

function buildUserPrompt(request: AiComponentRequest): string {
  const { prompt, context } = request
  const otherBindings = context.inScopeBindings.map((binding) =>
    formatBinding(binding.identifier, binding.typeName),
  )
  const otherBindingsList = otherBindings.length > 0 ? otherBindings.join('\n') : '(none)'
  const moduleImportsList =
    context.moduleImports.length > 0 ? context.moduleImports.join('\n') : '(none)'
  const sourceSection =
    context.sourceIdentifier != null ?
      `Source binding (the value the user wants to operate on):
${formatBinding(context.sourceIdentifier, context.sourceTypeName)}

`
    : ''
  return `Current method: ${context.currentMethodName}
Current method source:
\`\`\`
${context.currentMethodCode}
\`\`\`

Module imports (already in scope without qualification):
${moduleImportsList}

${sourceSection}Other in-scope bindings:
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

function truncateStderr(stderr: string): string {
  const trimmed = stderr.trim()
  if (trimmed.length <= STDERR_TAIL_CHARS) return trimmed
  return `…${trimmed.slice(-STDERR_TAIL_CHARS)}`
}

/**
 * Send a progress event to a specific renderer without going through the per-turn `pending` slot.
 * Used for `queued` acknowledgments emitted before the turn begins (and thus before `pending`
 * is set) — `ClaudeAgentSession.emitProgress` requires a matching pending requestId, which we
 * don't have yet at that point.
 */
function emitProgressTo(sender: WebContents, event: AiProgressEvent): void {
  if (sender.isDestroyed()) return
  try {
    sender.send(Channel.aiProgress, event)
  } catch {
    // Renderer destroyed mid-emit; the next send will short-circuit on isDestroyed().
  }
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
function streamJsonArgs(config: ClaudeSessionConfig): string[] {
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

// Probe-confirmed shape of the envelopes the CLI emits in stream-json mode. The format is
// undocumented (see https://github.com/anthropics/claude-code/issues/24594); discovery notes
// and per-turn emission order are in app/electron-client/CLAUDE.md.
//
// Each turn produces, in order: `system` (init), optional `rate_limit_event`, one `assistant`
// (with the reply text in `message.content[].text`), then the terminal `result` envelope. The
// schemas below validate only the fields we read; everything else is ignored.

/* eslint-disable camelcase -- mirrors the snake_case keys the CLI emits. */
const tokenUsageSchema = z.object({
  input_tokens: z.number().optional(),
  output_tokens: z.number().optional(),
  // Anthropic API reports the input split between non-cached and cache-served tokens; sum all
  // three to recover the actual context size the API saw for this completion.
  cache_creation_input_tokens: z.number().optional(),
  cache_read_input_tokens: z.number().optional(),
})
/* eslint-enable camelcase */

// `text` blocks carry the model's natural-language output; `tool_use` blocks carry the model's
// decision to call a tool (`name` is the tool name, `input` is the args payload). The fields
// below cover both shapes so we can route each block in `captureAssistantContent` without a
// second parse — anything else is ignored. The CLI may also surface `message.usage` on each
// envelope (Anthropic Messages API standard); when present it's the *per-hop* `usage` for the
// completion call that produced this assistant message — captured to estimate the actual
// context window occupancy of the turn's final synthesis call. Optional because some CLI
// versions / envelope shapes may omit it.
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

type RawTokenUsage = z.infer<typeof tokenUsageSchema>

interface TurnOutcome {
  state: 'completed' | 'crash'
  text: string
  usage: RawTokenUsage | null
  /** `usage` from the final `assistant` envelope; `null` when none seen / CLI omitted it. */
  lastHopUsage: RawTokenUsage | null
  /** Number of `assistant` envelopes seen this turn. `0` for crash-before-first-response. */
  hopCount: number
  /**
   * Wall-clock ms from the moment we wrote the user turn to stdin until the turn settled
   * (completed, crashed, timed out, or was cancelled). `0` means the turn never started — the
   * child wasn't alive when {@link runOneTurn} was called, so there was no clock to measure.
   */
  durationMs: number
  errorReason?: string
}

interface PendingTurn {
  /** Renderer-supplied id, or {@link PRIMING_REQUEST_ID} for priming. */
  readonly requestId: string
  resolve: (outcome: Omit<TurnOutcome, 'durationMs' | 'lastHopUsage' | 'hopCount'>) => void
  textChunks: string[]
  /** Pinned per turn (not per session) so crash/shutdown drop the slot for free; `null` for priming. */
  sender: WebContents | null
  /**
   * Most recent `assistant` envelope's `message.usage` we observed in this turn. Used as the
   * "current context window occupancy" signal at turn end. `null` when the CLI doesn't surface per-envelope
   * `usage`.
   */
  lastHopUsage: RawTokenUsage | null
  /** Number of `assistant` envelopes seen this turn. */
  hopCount: number
}

/** Renderer + request id driving a turn. */
export interface ActiveRequest {
  readonly requestId: string
  readonly sender: WebContents
}

// ============================
// === ClaudeAgentSession ===
// ============================

/**
 * Owns a single long-lived `claude` subprocess: spawn + priming run in the background, requests
 * serialize through a FIFO queue, and the child is auto-respawned on unexpected exit (with a
 * crash-loop guard).
 */
export class ClaudeAgentSession {
  private readonly config: ClaudeSessionConfig
  private readonly watcher: WatchedChildProcess
  private readyDeferred: Deferred<void> = createDeferred()
  private readonly queue = new AsyncQueue<void>(Promise.resolve())
  private pending: PendingTurn | null = null
  private stderrTail = ''
  private disposed = false
  /** Ids cancelled while still queued behind another turn; consumed by the queue task on entry. */
  private readonly cancelled = new Set<string>()
  /**
   * Ids currently in the queue or running. Populated synchronously in {@link runRequest} before
   * the task is pushed, cleared in the task's `finally` once it settles.
   */
  private readonly liveRequests = new Set<string>()
  /**
   * Empty temp dir used as the spawned `claude` process's cwd. Without it the child inherits
   * Electron's cwd and the agent's `Read` tool can reach arbitrary paths under it (user home,
   * `/tmp`, ...). Pairing an empty cwd with `--add-dir <stdlibRoot>` confines `Read`/`Glob`/
   * `Grep` to the stdlib, forcing the agent to use `evaluateExpression` for any user-data
   * inspection — which is the right boundary anyway, since user data lives in the engine and
   * is only correctly observable through the LS.
   */
  private readonly sandboxCwd: string

  /** Spawn the child eagerly and kick off the priming turn in the background. */
  constructor(config: ClaudeSessionConfig) {
    this.config = config
    this.sandboxCwd = fs.mkdtempSync(path.join(os.tmpdir(), 'enso-claude-cwd-'))
    // Attach a no-op rejection handler so that a crash mid-priming doesn't surface as an
    // unhandled rejection when no `runRequest` happens to be awaiting `ready` at the time.
    // Awaiters that arrive later attach their own .then/.catch and still observe the rejection.
    this.readyDeferred.promise.catch(() => undefined)
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
   * The renderer + request id driving the in-flight turn, or `null` between turns / when the
   * renderer was destroyed.
   */
  get activeRequest(): ActiveRequest | null {
    const pending = this.pending
    if (pending == null) return null
    const sender = pending.sender
    if (sender == null || sender.isDestroyed()) return null
    return { requestId: pending.requestId, sender }
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
          if (this.watcher.respawnSuspended && !this.watcher.current?.alive) {
            // The watcher's recent-exits buffer is preserved across respawn(), so a quick
            // re-crash trips the guard again and we don't loop indefinitely.
            this.readyDeferred = createDeferred()
            this.readyDeferred.promise.catch(() => undefined)
            await this.watcher.respawn()
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
          const turn = await this.runOneTurn(
            buildUserPrompt(request),
            REQUEST_TIMEOUT_MS,
            sender,
            request.requestId,
          )
          resolveOuter(this.replyFromTurn(turn))
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
   * synchronously with a cancellation `Err` and the child is SIGINT'd (with a 2s SIGTERM watchdog
   * if SIGINT is ignored). Queued requests file the id in {@link cancelled} for the queue task to
   * short-circuit. Idempotent.
   */
  cancelTurn(requestId: string): void {
    if (this.disposed) return
    const pending = this.pending
    if (pending != null && pending.requestId === requestId) {
      this.pending = null
      pending.resolve({
        state: 'crash',
        text: '',
        usage: null,
        errorReason: 'cancelled by user',
      })
      this.signalCancel()
      return
    }
    // Only file a deferred cancel when the id is one we're tracking — cancels for ids that have
    // already settled, or that were never issued, would otherwise accumulate in the set forever.
    if (this.liveRequests.has(requestId)) {
      this.cancelled.add(requestId)
    }
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
    void this.watcher.close()
    try {
      fs.rmSync(this.sandboxCwd, { recursive: true, force: true })
    } catch {
      // Already gone or filesystem-level failure; nothing meaningful to do here.
    }
  }

  // -------------- private --------------

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
    // Reject the priming promise so any task awaiting `ready` fails fast. Prefer the original
    // error object (when 'error' fired on the child, e.g. ENOENT) so `formatNotReadyError` can
    // surface the install hint via `.code` instead of parsing the reason string.
    this.readyDeferred.reject(info.exitError ?? new Error(reason))

    if (info.exceedsCrashLimit) {
      console.warn(
        `[AI] claude crash-loop guard tripped (${MAX_RESPAWNS_IN_WINDOW} crashes within ${RESPAWN_WINDOW_MS}ms); auto-respawn is suspended until the next request.`,
      )
      // Defer to the watcher's default for exceedsCrashLimit (suspend).
      return
    }
    // Prepare a fresh deferred for the upcoming auto-respawn so awaiters that arrive between
    // here and `onChildStarted` see a pending promise instead of the just-rejected one.
    this.readyDeferred = createDeferred()
    this.readyDeferred.promise.catch(() => undefined)
  }

  private async prime(): Promise<void> {
    const outcome = await this.runOneTurn(
      buildPrimingPrompt(this.config),
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

  private runOneTurn(
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

  /** Drops silently when the pending slot has rotated or the originating sender is gone. */
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

  private replyFromTurn(turn: TurnOutcome): AiComponentIpcReply {
    const usage = this.snapshotUsage(turn.usage, turn.lastHopUsage, turn.hopCount, turn.durationMs)
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

  private snapshotUsage(
    raw: RawTokenUsage | null,
    lastHop: RawTokenUsage | null,
    hopCount: number,
    durationMs: number,
  ): RequestUsage | null {
    if (!raw) return null
    // Prefer the final assistant envelope's `usage` for the context-window signal — that's the
    // synthesis call's actual prompt size, the most-loaded state of the turn. Fall back to the
    // result-envelope sum only if the CLI didn't surface per-envelope `usage`, since the sum
    // overstates occupancy on multi-hop turns. The renderer warns on fallback when
    // `hopCount > 0` so the developer notices broken context telemetry.
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
 */
export function initClaudeAgentIpc(config: ClaudeSessionConfig): void {
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
        `[AI] '${CLAUDE_EXECUTABLE}' not found on PATH; AI node generation will fail until Claude Code is installed.`,
      )
    } else {
      console.warn(
        `[AI] failed to start '${CLAUDE_EXECUTABLE}' session: ${errno?.message ?? String(err)}`,
      )
    }
  })
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
