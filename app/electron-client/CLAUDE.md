# electron-client

The Electron main process. Package name: `enso`. Produces the installable
desktop binary (AppImage/DMG/exe + installer).

## Layout

- `src/` — Electron main process TypeScript.
- `buildInfo.ts`, `esbuildConfig.ts`, `bundle.ts`, `dist.ts`, `watch.ts` — build
  orchestration (bundles `src/` with esbuild, runs `electron-builder`).
- `electron-builder-config.cjs` / `electron-builder-config.ts` — packaging
  config (icons, code signing, entitlements).
- `macos/`, `entitlements.mac.plist` — macOS DMG/notarization extras.
- `tasks/` — one-shot CLI tasks exposed for `vite-node`.
- `assets/` — icons/images that end up in the installer.
- `paths.ts` — canonical path resolution (userData, logs, bundled engine).
- `fileAssociations.ts` — `.enso` file-association handlers.
- `export-config.mjs` — emits the final Vite config for packaging.

## What this process does

- Serves the GUI bundle (from `app/gui/dist/`) in a BrowserWindow.
- Runs the Project Manager in-process via the TS `project-manager-shim` (the
  Scala Project Manager is no longer used, not even in the packaged Electron
  build).
- Implements the custom `enso://` protocol handler used by Enso Cloud links.
- Handles deep-links, auto-update (electron-updater via electron-builder),
  custom window chrome.

## Build

Currently driven by `./run ide build` (the legacy Enso build CLI). Don't call
`electron-builder` or `pnpm run dist` manually unless you're debugging packaging
— the build CLI wires the engine bundle, GUI, and Electron together with the
right env vars.

**Always run `git submodule update --init --recursive` before building.** In
particular, `app/gui/.dev-env/` (a private submodule) holds `.env.staging` /
`.env.production` with the Cognito client ids and the staging cloud API URL. A
fresh worktree starts with that submodule empty. The build does **not**
hard-fail when it's missing — the symlink `app/gui/.env.staging` becomes broken,
vite silently falls back to defaults, and the resulting Electron app points the
dashboard at the production cloud regardless of `--mode staging`. The end-to-end
tests then fail at the login screen (`Loading Enso…` →
`Error while fetching configuration [Error: Failed to fetch]`) because the
in-renderer fetch hits a URL the auth flow can't handshake. Init the submodules
before any IDE build (and before running the e2e suite that loads that build).

For testing, always build with `--mode staging`, to not pollute production
backend and telemetry with test runs.

`./run` is slated to be replaced by a Bazel target; check for one before
assuming `./run` is the only path.

`watch:linux` / `watch:macos` / `watch:windows` scripts are for local iteration
once `./run ide build` has produced the engine bundle.

## Env vars of note

- `ENSO_BUILD_IDE`, `ENSO_BUILD_BACKEND` — output locations.
- `ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION` — which engine to bundle.
- `ENSO_POLYGLOT_YDOC_SERVER` — URL for the (polyglot) ydoc server when running
  against a cloud backend.
- `ENSO_IDE_VERSION`, `ENSO_IDE_COMMIT_HASH` — embedded into buildinfo.

## Local Claude agent

The agent code lives under `src/ai/`. `src/ai/claudeAgent.ts` owns
`ClaudeAgentSession` — the session-level orchestration (FIFO queue, IPC binding,
context-rotation policy). `src/ai/claudeAgentChild.ts` owns `ChildAgent` — the
per-child plumbing (one spawned `claude`, its priming turn, stream-json parsing,
signal-cancel, crash bookkeeping). System, priming, and per-turn user prompt
builders live in `src/ai/prompts.ts`. The session composes one (or, mid
context-rotation, two) `ChildAgent` instances; everything below describes how
they cooperate.

The session serves every AI-component IPC for the lifetime of the Electron app.
It is constructed eagerly (but non-blockingly) when `initClaudeAgentIpc(config)`
runs, the primary `ChildAgent` is primed with a warm-up turn that pre-loads
stdlib documentation into context (see "Priming" below), and the session (plus
any active warming child) is torn down via SIGTERM on `before-quit`. The CLI is
launched via `cross-spawn` (not `node:child_process`) so npm-installed Claude
Code on Windows — which arrives as `claude.cmd` — is resolved without forcing
`shell: true`.

Invocation flags:
`-p --input-format stream-json --output-format stream-json --verbose --system-prompt <SYSTEM_PROMPT> --add-dir <stdlibRoot> --allowedTools "Read,Glob,Grep" --setting-sources "" --no-session-persistence`.
`--verbose` is mandatory: omitting it makes the child exit 1 with
`When using --print, --output-format=stream-json requires --verbose`. The extra
system/init and rate_limit_event envelopes it emits are filtered by the parser.
We deliberately do **not** pass `--json-schema` here, even though the probe
showed it works in stream-json mode: it adds ~800 tokens of schema overhead per
turn plus an internal tool-use round-trip (~1s), and it conflicts with the
priming turn (which doesn't fit the AI component shape). The system prompt asks
for JSON-only output and `aiComponentResponseSchema.safeParse` validates on our
side. `--setting-sources ""` keeps the invocation hermetic (no user
settings/plugins/`CLAUDE.md` discovery) without touching auth; `--bare` is
deliberately avoided because it would re-introduce the `ANTHROPIC_API_KEY`
requirement.

`--add-dir <stdlibRoot>` plus `--allowedTools "Read,Glob,Grep"` lets the agent
browse the bundled standard library on disk when it's unsure of an API name or
signature — the system prompt's cheat sheet covers only the most common entry
points, and inventing a name is the dominant failure mode. `--allowedTools`
(camelCase, not the deprecated `--tools`) **pre-grants** the listed tools so no
permission prompt fires in `-p` mode (where there is no UI to prompt against).
The stdlib path is resolved at startup by `paths.stdlibRoot()`, which derives
from the PM shim's `findStdlibRoot()` (sibling of `findEnsoExecutable`,
`<engineRoot>/lib/Standard` where `<engineRoot>` is two `dirname`s above the
engine binary). When the binary cannot be located the path is `undefined`; the
session still spawns but without filesystem access and with the stdlib hint
omitted from the system prompt — the agent shouldn't be told it has tools it
can't actually use.

**Cwd sandboxing.** The spawned `claude` is launched with `cwd` set to a fresh
empty temp dir (`<tmpdir>/enso-claude-cwd-<rand>/`), removed on `shutdown()`.
The Read/Glob/Grep tools resolve filesystem operations against the cwd plus any
`--add-dir` paths; without an explicit cwd the child would inherit Electron's,
putting the user's home directory and `/tmp` within reach. The empty-dir cwd
plus `--add-dir <stdlibRoot>` confines those tools to the stdlib alone. User
data is never accessible by `Read` — it only flows back through
`evaluateExpression`, which is the right boundary anyway because user data lives
in the engine and is only correctly observable through the LS.

`IDLE_TIMEOUT_MS` (in `claudeAgent.ts`) is the per-turn inactivity cap: 5 min of
silence on the stream-json channel — no `assistant` envelope, no tool use —
fires the timeout. It resets on every `assistant` envelope (see
`captureAssistantContent`), so a turn that keeps narrating or fanning out tool
calls runs as long as it needs and the timeout fires only when the model is
genuinely stuck. Earlier revisions used a hard 360 s wall-clock cap (originally
120 s pre-tools); the wall-clock approach clipped legitimate long but healthy
turns. The current idle approach lets healthy turns run as long as the model
keeps producing output. The 5 min ceiling is sized to tolerate the worst-case
single thinking block on `--effort max`: the underlying API doesn't ship
per-token thinking deltas on the CLI's auth path (we probed it — only
`message_start` and `content_block_start:thinking` arrive at ~+3 s, then nothing
until the block closes), so a long thinking phase is, from our side, just
silence. Asking the model to "narrate more often" via the system prompt does not
help — it can't emit text mid-thinking — so the only correct knob is runtime
tolerance. `PRIMING_IDLE_TIMEOUT_MS` (in `claudeAgentChild.ts`) is the same 5
min — each priming Read fires its own `assistant` envelope, so realistic gaps
are sub-second; sustained idleness during priming is a stuck CLI.

### Priming

`buildPrimingPrompt(config)` returns one of two prompts. With `stdlibRoot`
available (the normal case), the priming user turn tells the agent two things.
First, Read three CLAUDE.mds — the top-level `<stdlibRoot>/CLAUDE.md` plus
`Base/0.0.0-dev/CLAUDE.md` and `Table/0.0.0-dev/CLAUDE.md`. Second, Glob and
Read every top-level `.enso` file under `Table/0.0.0-dev/src/` — the pattern is
`*.enso` (single `*`), so the format/IO subdirectories (`Internal/`, `Excel/`,
`Delimited/`, `Conversions/`, etc.) are deliberately skipped. Only after both
does the agent reply `READY`.

The split is intentional: the top-level file documents universal stdlib
conventions and per-library files describe library-specific surface only.
Pre-loading Table's top-level source (~36 files, ~12 K lines covering
`Table.enso`, `Column.enso`, `Aggregate_Column.enso`, `Value_Type.enso`,
`Expression.enso`, `Join_Condition`/`Join_Kind`, and the small spec types)
covers the entire Table public surface the test suites exercise. Earlier
revisions of this priming used `Image/0.0.0-dev/src/**/*.enso` (~9 files, ~64
kB) as a syntax demo, but Image is not actually used by the test surface and the
swap to Table gave the agent first-class signatures and doc blocks for the
methods it actually invokes. The subdirectory format/IO modules and other
libraries are loaded on demand: the system prompt instructs the agent to Read
`<Name>/0.0.0-dev/CLAUDE.md` (and source files under that library) the first
time it needs them, and the result then lives in context for the rest of the
session.

The Reads' results live in the conversation context for every subsequent turn —
once primed, the agent doesn't need to re-Read these files; the per-turn input
cost they add is what `[AI] usage:` `context=` reports. The fallback prompt (no
stdlib) is still the one-line "say READY" form — when the agent has no Read
access there is nothing to ingest.

Priming runs with `sender == null`, so `evaluateExpression` would fail in the
priming turn; that's fine — Read/Glob/Grep are CLI-internal tools and need no
renderer. If a per-library `CLAUDE.md` happens to be missing (older engine
bundle), the priming Reads degrade fail-soft (the missing files return a clean
error, the agent continues). The system prompt's stdlib bullets describe this
same material (`## ` doc blocks, `Internal/` and `private: true` warnings, the
per-library CLAUDE.md location), so even if priming partially fails the agent
still has the high-level rules in front of it.

### AI tool bridge (MCP `evaluateExpression`)

`src/ai/aiMcpServer.ts` owns an in-process MCP server bound to a random
localhost port at app startup. Its single tool,
`evaluateExpression(expression)`, lets the model run a plain Enso expression in
the scope where the AI's new node would land — exactly the same scope the
generated `body` will see, so every in-scope binding listed in the prompt is
referenceable by name (and one call can stitch several of them together, e.g.
`(cards.join leader_order on=["Set"]).column_names.to_json`). The server writes
a temporary JSON config file (`<tmpdir>/enso-claude-mcp-<pid>-<uuid>.json`) and
that path is passed to the agent as `--mcp-config <path> --strict-mcp-config`;
`--strict-mcp-config` makes the CLI ignore any project- or user-level MCP config
so the session is hermetic. The temp file is deleted on shutdown.

**Text-only return contract:** the LS evaluates the expression `inFrame` and the
engine's `VisualizationResult.visualizationResultToBytes` only knows how to
encode `Text`/`String` values. Wrapping the agent's expression on our side (e.g.
`Standard.Visualization.Preprocessor.default_preprocessor`) was rejected because
it forces a stringification cost on every call and the Visualization module
isn't reliably resolvable in inline scope. Instead the system prompt tells the
agent to choose its own encoding (`.to_text`, `.to_display_text`, `.to_json`, …)
and `aiToolHandler.ts` translates the engine's
`Cannot encode class X to byte array.` into an actionable hint ("Expression must
evaluate to Text … wrap with `.to_text` … use `.catch_primitive` for failing
expressions") so a slip-up teaches rather than mystifies. Future structured-data
needs (small images, sample bytes) get a new tool variant; the text path stays
strictly text.

**Why HTTP (not stdio):** Claude Code can spawn its own MCP servers via stdio,
but the server we need has to share state with the Electron main process —
specifically, it must reach into the renderer's `graphDb`/`executionContext` to
evaluate expressions. Hosting in-process and pointing the CLI at it via
`{"type":"http","url":"http://127.0.0.1:<port>/mcp"}` sidesteps cross-process
state-sharing entirely. The transport uses `StreamableHTTPServerTransport` from
`@modelcontextprotocol/sdk` in stateless mode (each request gets its own
short-lived transport — the single in-process consumer doesn't keep an SSE
channel open between turns).

**Per-turn sender slot:** the agent's singleton serves any window in the app,
but a tool call needs to dispatch back to the _specific_ renderer that
originated the in-flight turn. `runRequest(request, sender)` pins the
`WebContents` to the per-turn `pending` slot (not to the session — that way
crashes and `shutdown()` reject in-flight bridge promises by construction). The
MCP server reads it via `session.activeRequest`, which returns `null` between
turns and also when the in-flight sender has since been destroyed. Tool calls
that find `null` reply with a clean `Err("no active AI turn …")` so the model
recovers cleanly instead of hanging.

**Reentrancy contract:** within a turn the model can fan out tool calls
sequentially, but the outer `AsyncQueue` in `ClaudeAgentSession.runRequest`
guarantees only one turn is ever in flight at a time — so multiple windows
serialize through the singleton, and a second window's IPC blocks until the
first turn returns. Acceptable today (the typical user has one window); if
multi-window AI becomes common the `pending` slot would need to grow into a
per-turn map.

**Timeouts:** per-tool-call 30 s on the main-process side (the MCP server
rejects with a clean error after that), nested inside the per-turn
`IDLE_TIMEOUT_MS` (5 min of channel inactivity — see "Local Claude agent"
above). The 30 s per-tool-call cap also keeps a hung renderer evaluation from
silently consuming the outer idle window, since a tool result whose dispatch
times out lets the model produce its next `assistant` envelope and that resets
the idle timer. The `--allowedTools` list is built dynamically: `Read,Glob,Grep`
are added when the stdlib path is available, `mcp__enso__evaluateExpression` is
added when the MCP server started successfully — the system prompt's "Tools you
have available" list mirrors that, so we don't lie to the model about
capabilities that aren't wired.

**Renderer side:**
`app/gui/src/project-view/components/ComponentBrowser/aiToolHandler.ts` exposes
a `useAiToolHandler()` Vue composable mounted by `GraphEditor.vue` so the IPC
subscription lives for the whole project session — the Component Browser closes
on AI-prompt submission and would otherwise tear the handler down before the
agent issues its first tool call. It subscribes to `window.api.ai.onToolCall`,
looks up the originating placeholder via
`aiPrompts.findByRequestId(request.aiRequestId)`, and uses the entry's
`methodBodyId` (captured at enqueue) as the LS scope-anchor — pinning per
request rather than per current view, so a method-navigation while the turn is
in flight does not silently move tool calls into the wrong scope. See the
scope-semantics docstring at the top of `aiToolHandler.ts` for why the body's
`externalId` is the right anchor and why graph node ids are not. It then calls
`queuedExecuteExpressionRaw(anchor, expression)` from the project store (the
**raw** variant — JSON parsing is intentionally bypassed so the agent controls
the encoding; the queued variant cooperates with the `MAX_IN_PROGRESS=5` cap and
retry/backoff in `project.ts`), and forwards the UTF-8-decoded text through
`replyToolCall`. Failure paths run through `translateEngineError` so the
engine's raw `Cannot encode class X to byte array.` becomes a self-teaching
hint. Returns a clean `Err` for "no active project", "current method has no
parsed AST", and "current method has no body to anchor scope". The slot
machinery in `project.ts:awaitExecuteSlot` resolves with `Err(message)` on
`failed`/timeout (rather than rejecting) so the `Result<string>` contract is
consistent across success and failure paths and `aiToolHandler.ts`'s
`if (!result.ok)` branch catches every legitimate evaluation failure.

The renderer reaches the IPC via `window.api.ai.generateComponent(...)` (see
`enso-gui/src/electronApi.ts`) over channel `Channel.generateAiComponent`. The
shared request/response types live in `enso-common/src/ai.ts`. The IPC return
shape is
`AiComponentIpcReply = { result: Result<AiComponentResponse>, usage: RequestUsage | null }`
— `result` carries the parsed/validated component (or an error), and `usage`
carries `inputTokens`/`outputTokens` plus `contextTokens` (the API-reported
input size for this turn — see "Context tokens" below). The renderer logs a
one-line `[AI] usage:` summary to its DevTools console so context growth is
observable on real data. At main-process startup `initClaudeAgentIpc()` attaches
a one-time diagnostic to `session.ready` so a missing CLI logs an install hint
immediately, without spawning a separate `--version` probe — the session itself
surfaces ENOENT through the watcher (synchronous spawner throws via
`firstSpawn`, async `'error'` events captured by `ChildProcessHandle`'s
`exitError` and forwarded through `UnexpectedExitInfo`). The first real IPC call
surfaces the same error to the renderer as a toast.

The agent generates a full User Defined Component, returning four fields:
`functionName`, `argumentNames`, `body`, and `callArguments`. The renderer
collects all in-scope bindings in the current method (with their inferred
types), the method's source code, the verbatim text of every `import` statement
at the top of the module, and — optionally — a source binding the user dropped
into the prompt, then passes that context to the agent so it can pick which
bindings to thread into the function. The module-imports list tells the agent
which names resolve unqualified (so it doesn't reach for an unimported atom by
its short name and produce a `name X could not be found` compile error); the
agent cannot mutate imports itself, so when it needs an atom whose type isn't
imported, the system prompt directs it to use the `..` auto-resolve constructor
form rather than fully qualified names. The source binding is optional: when
absent, the agent generates a component from scratch using only the other
in-scope bindings (or none at all). `argumentNames` are pure function-signature
parameter names (referenced by `body`); `callArguments` are the Enso expressions
passed at the call site, one per parameter. `createAiNode`
(`app/gui/src/project-view/components/GraphEditor/aiNode.ts`) parses each entry
of `callArguments` with the Enso parser and assembles
`Main.<functionName> <callArguments[0]> <callArguments[1]> …` itself, so the
agent never has to construct call-site syntax — keeping the response purely
data-shaped removed the AST traversal that the prior `validateCallExpression`
needed.

### Stream-json wire format (probe-confirmed)

The CLI's stream-json envelope is undocumented (see
[claude-code#24594](https://github.com/anthropics/claude-code/issues/24594)).
Findings from the probe at the time the long-lived design landed:

- **Stdin (per turn):**
  `{"type":"user","message":{"role":"user","content":<string>}}\n`. JSONL
  framing, newline-terminated. `content` as a plain string is accepted.
- **Stdout (per turn, in order):**
  1. `{"type":"system","subtype":"init", ...}` — large session-init payload that
     **repeats every turn**, not just at startup. Filter on `type==='system'`.
  2. `{"type":"rate_limit_event", ...}` — emitted at least on the first turn.
     Filter.
  3. `{"type":"assistant","message":{...,content:[...],usage:{...}}}` — one
     event per completion call. A multi-hop turn (with tool_use loops) emits one
     assistant envelope per hop, each carrying `message.usage` for that specific
     completion. The final envelope before the result holds the synthesis call's
     usage and is the source for `RequestUsage.contextTokens`.
  4. `{"type":"result","subtype":"success","is_error":false,"terminal_reason":"completed", "result":<text>,"usage":{...},...}`
     — terminal envelope. `result.usage` carries `input_tokens`,
     `output_tokens`, `cache_creation_input_tokens`, and
     `cache_read_input_tokens`. This is the unambiguous end-of-turn signal.
- **Multi-turn:** the same child accepts subsequent turns; `session_id` is
  stable and conversation history is maintained on the CLI side.
- **Malformed stdin is fatal:** sending an unparsable line makes the CLI emit
  `Error parsing streaming input line: …` on stderr and `exit 1`. We always
  write `JSON.stringify(...)`, so we never trigger this — the crash-respawn path
  covers it if it ever happens.
- **SIGTERM is graceful:** the child exits in ~220ms with code 143. No SIGKILL
  fallback is needed in `shutdown()`.
- **No unsolicited stdout between turns**: the session is silent until we send
  the next user turn.

### Runtime behavior

- **FIFO queue:** the renderer's `aiPrompts` store enforces single-in-flight at
  the window level, but overlapping IPC calls (multi-window, or main-process
  priming overlap) are still serialized through a shared `AsyncQueue` from
  `enso-common/src/utilities/async`. Only one stdin write is in flight at a
  time.
- **Cancellation:** `Channel.cancelAiComponent` carries the renderer's
  `requestId`. On match against the pending slot, the session nulls pending
  synchronously, resolves the originating turn with
  `Err('Claude agent: cancelled by user')`, and writes an SDK-shaped
  `control_request`/`interrupt` envelope to the child's stdin
  (`{"type":"control_request","request_id":"req_…","request":{"subtype":"interrupt"}}`).
  Modern Claude Code (2.x+ stream-json mode) replies with `control_response`
  `subtype: success` within milliseconds and emits an
  `aborted_streaming`/`is_error: true` `result` envelope for the cancelled turn
  — **without exiting the child**, so the next turn reuses the same primed
  conversation context. Wire format and timing confirmed via
  `/tmp/claude-interrupt-probe.mjs` (interrupt→result was 3 ms; the follow-up
  trivial turn completed on the same primed child with no re-prime). For a
  queued request that hasn't reached `runOneTurn` yet, the id is filed in a
  `cancelled` set; the queue task consumes it on entry and short-circuits
  without any envelope written.

  **Fallback ladder.** If no `control_response` arrives within
  `CANCEL_CONTROL_FALLBACK_MS` (1000 ms), or if the CLI replies
  `subtype: error`, `escalateSignalCancel` takes over: SIGTERM now, SIGKILL
  after `CANCEL_SIGTERM_TO_SIGKILL_MS` (2000 ms) if the child is still alive.
  SIGINT is intentionally skipped — once we've decided to escalate, the goal is
  a guaranteed exit so the watcher can respawn; SIGTERM is the polite shutdown
  (the child exits in ~220 ms with code 143 — see the stream-json wire-format
  section), and SIGKILL is the hammer for the rare case SIGTERM is ignored. The
  escalation path also pre-swaps `readyDeferred` and arms the `cancelInProgress`
  flag (since the child _does_ exit on this path) so the next queue task's
  `await primary.ready` synchronizes on the upcoming respawn's prime instead of
  racing past a stale resolved deferred. `onUnexpectedExit` honors the flag and
  leaves the new pending deferred alone (the auto-respawn's `onChildStarted` →
  `prime` resolves it). The common path doesn't touch `readyDeferred` — the
  child stays alive and primed.

  **Aborted-result filter.** After a successful interrupt the CLI emits a
  `result` envelope with `is_error: true` and
  `terminal_reason: aborted_streaming` on the same stdout stream as future
  turns' results. `resolveTerminal` drops these explicitly so a late-arriving
  aborted envelope can't tear down the next turn's pending state.

  Regression tests live in `tests/headless/claudeAgentChild.test.ts`
  (`cancelInFlight writes a control_request interrupt envelope …`,
  `cancelInFlight: control_response success keeps the child usable …`,
  `cancelInFlight: no control_response within the fallback window escalates to SIGTERM`,
  `cancelInFlight: control_response error escalates to SIGTERM immediately`) and
  `tests/headless/claudeAgent.test.ts`
  (`cancelTurn on the in-flight request resolves with cancellation Err and writes a control_request`,
  `a queued request submitted right after a cancel runs on the SAME child …`);
  end-to-end behaviour is in `tests/aiNode.spec.ts`
  (`cancelling a running AI prompt leaves the queue healthy …`).

- **Crash recovery:** an unexpected child exit fails any in-flight request with
  a structured `Err(...)`, then auto-respawns and re-primes. A crash-loop guard
  suspends auto-respawn after 3 unexpected exits within 30 seconds; the next IPC
  call attempts one more spawn before failing fast (so the user can recover by
  retrying after fixing the underlying issue).
- **Per-request idle timeout** (`IDLE_TIMEOUT_MS`, 5 min) returns
  `Err("no feedback for 300000ms (idle timeout)")` to the renderer but does
  **not** kill the still-warm child — the next request will reuse it. The late
  reply from the timed-out turn is dropped by the parser (`pending` is null).
  The timer resets on every `assistant` envelope, so a turn keeps running as
  long as the model produces output; it fires only when the channel stays silent
  for a full 5 min window.
- **Live progress:** `Channel.aiProgress` carries `AiProgressEvent`s tagged with
  the originating `requestId`. `queued` fires once the IPC reaches the
  AsyncQueue (acknowledging receipt — useful when a previous turn hasn't yet
  released the queue slot, or when priming is still finishing); `started` fires
  once stdin has been written; `text` fires for every non-empty text block in an
  `assistant` envelope; `tool` fires for every `tool_use` block (covers both
  built-in `Read`/`Glob`/`Grep` and the MCP `evaluateExpression`). The
  renderer's `aiPrompts` store routes each event to the placeholder it created
  and updates the visible status text. `emitProgress` filters events whose
  `requestId` doesn't match the current pending slot, so a stale event from a
  rotated/cancelled turn never lands on a new placeholder; the `queued` emit
  uses a separate `emitProgressTo(sender, …)` path because it must fire _before_
  the pending slot is set.
- **Context vs. cost split in `RequestUsage`.** Every `assistant` envelope the
  CLI emits carries Anthropic's per-completion `message.usage`; the terminal
  `result` envelope carries a roll-up that is empirically the **sum across all
  hops in the turn** (model → `tool_use` → `tool_result` → model continues …).
  We capture both:

  - `contextTokens` =
    `input_tokens + cache_read_input_tokens + cache_creation_input_tokens` from
    the **last** assistant envelope before the result. That is the synthesis
    call's actual prompt size, the most-loaded state of the turn — the right
    number to compare to a context window. It grows approximately monotonically
    across turns. If the final envelope omits `usage` (`captureAssistantContent`
    overwrites `lastHopUsage` with `null` rather than coalescing, so a stale
    earlier value never leaks through), this falls back to the result-envelope
    sum. The renderer's `logUsage` flips `ctxSrc=fallback` and emits a
    `console.warn`; `aiMetrics.appendMetricsRow` flags such rows by suffixing
    the `status` column with ` (broken)` (yielding `pass (broken)` or
    `fail (broken)`) so downstream analysis can filter them out without losing
    the rest of the run's signal.
  - `contextFromLastHop` exposes the source of `contextTokens` so consumers can
    validate; see above for how the metrics writer surfaces it.
  - `inputTokens`, `outputTokens`, `cacheReadTokens`, `cacheCreationTokens`,
    `hopCount` are turn totals from `result.usage` (and the count of assistant
    envelopes seen). They are the right inputs for billing and for explaining
    why a heavy-hops turn cost what it did.

  The renderer logs both:
  `prompt=…t out=…t context=<n.n>k hops=N ctxSrc=<lastHop|fallback> (cacheRead=…t cacheCreate=…t) time=…ms`.
  Conversation history accumulates in CLI process memory across the session
  (there is no auto-compact in `-p` mode — that's an interactive-mode feature),
  so `contextTokens` resets only on child respawn, process restart, or a
  context-threshold rotation (see below).

### Context-threshold rotation

Conversation history grows across turns; left unbounded the agent eventually
hits the model's context window. The session watches each turn's
`RequestUsage.contextTokens` (preferred from the final assistant envelope's
`message.usage`; falls back to the result-envelope sum when the CLI omits it)
and rotates to a fresh `claude` child once it crosses one of two thresholds:

- **Soft threshold** (default 300_000 tokens, override with
  `ENSO_AI_SOFT_CONTEXT_THRESHOLD`). A second `ChildAgent` ("warming") is
  spawned in the background and starts priming. The primary continues to serve
  new turns. Once the warming child's priming completes, the **next** queue task
  promotes it (old primary is SIGTERM'd inside `oldPrimary.shutdown()`).
- **Hard threshold** (default 400_000 tokens, override with
  `ENSO_AI_HARD_CONTEXT_THRESHOLD`). The warming child is spawned (if not
  already) and `swapMode` flips to `'hard'` — new queue tasks
  `await warming.ready` before running, so no further turn is sent to the old
  primary.

Threshold evaluation runs in the queue task's `finally` block, so it operates on
the **just-finished** turn's usage. The next queue task observes the new
`swapMode` at its task-start gate. As a consequence, **already-running turns are
never cancelled by a rotation** — the in-flight HTTPS stream to Anthropic on the
old primary always completes normally; only future turns are gated.

If `hard < soft` after env-var resolution, both fall back to defaults with a
warning. If env-var values aren't valid positive integers, that var alone falls
back to its default.

### Pass-through CLI flags (`ENSO_AI_CLAUDE_EXTRA_ARGS`)

`ENSO_AI_CLAUDE_EXTRA_ARGS` is split on whitespace and appended verbatim to the
spawned `claude -p …` flag list, after the built-in flags. Used by the
AI-effectiveness suite (`tests/aiChallengePrep.spec.ts`) to compare models and
reasoning levels — e.g. `ENSO_AI_CLAUDE_EXTRA_ARGS="--model claude-sonnet-4-6"`.
No shell-style quoting: values containing whitespace aren't expressible. Args
are forwarded to both the primary and any warming child so a context rotation
preserves the user-selected model.

**Failure handling.** Warming priming can fail (transient CLI bug, ENOENT,
crash-loop guard tripped). The session reacts:

- In **soft mode**, the next queue task's `promotePrimedWarming()` notices
  `warming.respawnSuspended && !warming.alive` and discards the warming child
  with a warning. `swapMode` resets to `'none'`. The next over-threshold turn
  re-arms a fresh warming attempt.
- In **hard mode**, the queue task currently `await`ing `warming.ready` catches
  the rejection, calls `warming.shutdown()` (which sets the watcher's `closed`
  flag so any pending auto-respawn aborts), nulls the warming slot, and resets
  `swapMode` to `'none'` — the request **falls back to primary** rather than
  failing the user's prompt over a transient priming flake.

**Two children, one queue.** The `AsyncQueue` is session-level: only one turn
ever runs at a time across both children. That keeps per-child `pending` slots
simple — primary's stdout drives primary's pending, warming's stdout drives
warming's (which during priming carries `sender: null` so it doesn't collide
with `activeRequest`'s "is a real request in flight?" predicate).
`session.activeRequest` returns
`primary.activeRequest ?? warming?.activeRequest ?? null` — the MCP server reads
this to dispatch tool calls back to the correct renderer regardless of which
child the turn happens to be running on.

### Gotchas

- The CLI lookup uses `cross-spawn`, not `node:child_process`, so npm-installed
  Claude Code on Windows (`claude.cmd`) resolves without `shell: true`. The unit
  test mocks `cross-spawn` (default export) rather than `node:child_process`.
- Electron IPC serializes with structured clone, which strips class prototypes.
  `Err(...)` from `enso-common/src/utilities/data/result` arrives at the
  renderer as a plain `{ payload, context }` — `ResultError`'s methods are gone.
  The renderer half (`ai.ts`) rebuilds the `Result` with `Ok()` / `Err()` right
  after the IPC call so downstream callers see a well-formed error.
- `system-init` envelopes contain large payloads (cwd, model, tools) and are
  emitted for **every** turn, not just the first. The parser must filter them
  out by `type` rather than treating the first one as a one-shot startup event.
- `--json-schema` does work with stream-json mode if you ever want to re-enable
  it; the validated payload arrives in `result.structured_output` (with `result`
  set to `""`) and the assistant content becomes a `StructuredOutput` `tool_use`
  block followed by an internal `tool_result` user echo. Be ready to filter the
  internal echo on `type==='user'`.

## Tests

Two layers, same directory, different runners — controlled by the `testIgnore`
rule in `playwright.config.ts`:

- `tests/headless/*.test.ts` — Vitest unit tests for main-process code. No
  Electron, no DOM. Fast. Run with `corepack pnpm vitest --run tests/headless`.
- `tests/*.spec.ts` — Playwright end-to-end tests that launch the packaged
  Electron binary from `dist/ide/` and drive the app from a real user's
  perspective (login → dashboard → project → graph editor). `electronTest.ts`
  extends Playwright's `test` fixture to spawn Electron and exposes helpers like
  `loginAsTestUser`, `createNewProject`, `openComponentBrowser`. See
  `tests/README.md` for prerequisites (a built `dist/ide/`, credentials at
  `playwright/.auth/user.json`). If on a worktree, Run with
  `corepack pnpm -r --filter enso ide-integration-test [path.spec.ts]`. Runs are
  long (minutes), so avoid them in inner dev loops.

When adding a Playwright test that needs an external dependency the CI doesn't
have yet (e.g. `aiNode.spec.ts` needs the local `claude` CLI), gate the whole
describe block on an env flag (`process.env.ENSO_TEST_AI === '1'`) and note the
flag in the plan's verification section so per-step smokes still exercise it
locally.

`tests/aiChallengePrep.spec.ts` is the heavy AI suite — it drives full Preppin'
Data challenge solves through Component Browser AI-mode prompts. It's gated on
`ENSO_TEST_AI_CHALLENGES_DIR=/abs/path` pointing at manually-downloaded inputs
(see `tests/README.md` for the expected layout) because the inputs aren't
checked in and the agent budget is real.
