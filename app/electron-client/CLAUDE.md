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
right env vars. `./run` is slated to be replaced by a Bazel target; check for
one before assuming `./run` is the only path.

`watch:linux` / `watch:macos` / `watch:windows` scripts are for local iteration
once `./run ide build` has produced the engine bundle.

## Env vars of note

- `ENSO_BUILD_IDE`, `ENSO_BUILD_BACKEND` — output locations.
- `ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION` — which engine to bundle.
- `ENSO_POLYGLOT_YDOC_SERVER` — URL for the (polyglot) ydoc server when running
  against a cloud backend.
- `ENSO_IDE_VERSION`, `ENSO_IDE_COMMIT_HASH` — embedded into buildinfo.

## Local Claude agent

`src/claudeAgent.ts` owns a **single long-lived `claude` CLI subprocess** that
serves every AI-component IPC for the lifetime of the Electron app. The session
is constructed eagerly (but non-blockingly) when `initClaudeAgentIpc(config)`
runs, primed with a small acknowledgment turn so the system prompt is ingested
up-front, and torn down via SIGTERM on `before-quit`. The CLI is launched via
`cross-spawn` (not `node:child_process`) so npm-installed Claude Code on Windows
— which arrives as `claude.cmd` — is resolved without forcing `shell: true`.

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

`REQUEST_TIMEOUT_MS` was bumped from 120 s (pre-tools) to 360 s — a single turn
now does up to a handful of stdlib lookups plus `evaluateExpression` round-trips
on top of the model's own output, and tighter budgets started clipping
legitimate turns. The priming timeout (60 s) and the per-stdin write
retry/backoff are unchanged.

### AI tool bridge (MCP `evaluateExpression`)

`src/aiMcpServer.ts` owns an in-process MCP server bound to a random localhost
port at app startup. Its single tool, `evaluateExpression(expression)`, lets the
model run a plain Enso expression in the scope where the AI's new node would
land — exactly the same scope the generated `body` will see, so every in-scope
binding listed in the prompt is referenceable by name (and one call can stitch
several of them together, e.g.
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
MCP server reads it via `session.activeSender`, which returns `null` between
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
rejects with a clean error after that), nested inside the 360 s outer
`REQUEST_TIMEOUT_MS`. So the model's worst case is "spent the whole turn on tool
calls, none replied" — which still leaves room for it to wrap up. The
`--allowedTools` list is built dynamically: `Read,Glob,Grep` are added when the
stdlib path is available, `mcp__enso__evaluateExpression` is added when the MCP
server started successfully — the system prompt's "Tools you have available"
list mirrors that, so we don't lie to the model about capabilities that aren't
wired.

**Renderer side:**
`app/gui/src/project-view/components/ComponentBrowser/aiToolHandler.ts` exposes
a `useAiToolHandler()` Vue composable mounted by `ComponentBrowser.vue`. It
subscribes to `window.api.ai.onToolCall`, resolves the LS scope-anchor (the
current method body's `externalId`, mirroring the `ComponentBrowser.vue` preview
path — see the long-form scope-semantics docstring at the top of
`aiToolHandler.ts` for why this is the right anchor and why graph node ids are
not), calls `queuedExecuteExpressionRaw(anchor, expression)` from the project
store (the **raw** variant — JSON parsing is intentionally bypassed so the agent
controls the encoding; the queued variant cooperates with the
`MAX_IN_PROGRESS=5` cap and retry/backoff in `project.ts`), and forwards the
UTF-8-decoded text through `replyToolCall`. Failure paths run through
`translateEngineError` so the engine's raw
`Cannot encode class X to byte array.` becomes a self-teaching hint. Returns a
clean `Err` for "no active project", "current method has no parsed AST", and
"current method has no body to anchor scope". The slot machinery in
`project.ts:awaitExecuteSlot` resolves with `Err(message)` on `failed`/timeout
(rather than rejecting) so the `Result<string>` contract is consistent across
success and failure paths and `aiToolHandler.ts`'s `if (!result.ok)` branch
catches every legitimate evaluation failure.

The renderer reaches the IPC via `window.api.ai.generateComponent(...)` (see
`enso-gui/src/electronApi.ts`) over channel `Channel.generateAiComponent`. The
shared request/response types live in `enso-common/src/ai.ts`. The IPC return
shape is
`AiComponentIpcReply = { result: Result<AiComponentResponse>, usage: RequestUsage | null }`
— `result` carries the parsed/validated component (or an error), and `usage`
carries `inputTokens`/`outputTokens` plus the session's cumulative context-byte
total for that turn. The renderer logs a one-line `[AI] usage:` summary to its
DevTools console so context growth is observable on real data. Cache-hit fields
are intentionally not surfaced — the CLI's stream-json mode doesn't engage
Anthropic prompt caching (see "Stream-json wire format" below), so there is
nothing useful to log there. At main-process startup `initClaudeAgentIpc()`
attaches a one-time diagnostic to `session.ready` so a missing CLI logs an
install hint immediately, without spawning a separate `--version` probe — the
session itself surfaces ENOENT through the watcher (synchronous spawner throws
via `firstSpawn`, async `'error'` events captured by `ChildProcessHandle`'s
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
  3. `{"type":"assistant","message":{...,content:[{"type":"text","text":"..."}]}}`
     — the assistant reply (one event in non-partial mode).
  4. `{"type":"result","subtype":"success","is_error":false,"terminal_reason":"completed", "result":<text>,"usage":{...},...}`
     — terminal envelope. `result.usage` carries `input_tokens` and
     `output_tokens` (the cache-related fields are also present but always 0;
     see the prompt-caching note). This is the unambiguous end-of-turn signal.
- **Multi-turn:** the same child accepts subsequent turns; `session_id` is
  stable and conversation history is maintained on the CLI side.
- **Prompt caching does NOT auto-engage** in stream-json mode —
  `cache_read_input_tokens` is observed at 0 even with a substantial system
  prompt. The win from the long-lived design is cold-start avoidance, not cache
  reuse.
- **Malformed stdin is fatal:** sending an unparsable line makes the CLI emit
  `Error parsing streaming input line: …` on stderr and `exit 1`. We always
  write `JSON.stringify(...)`, so we never trigger this — the crash-respawn path
  covers it if it ever happens.
- **SIGTERM is graceful:** the child exits in ~220ms with code 143. No SIGKILL
  fallback is needed in `shutdown()`.
- **No unsolicited stdout between turns**: the session is silent until we send
  the next user turn.

### Runtime behavior

- **FIFO queue:** overlapping IPC calls (possible because the renderer's
  `processingAIPrompt` gate is per-window) are serialized through a shared
  `AsyncQueue` from `enso-common/src/utilities/async`. Only one stdin write is
  in flight at a time.
- **Crash recovery:** an unexpected child exit fails any in-flight request with
  a structured `Err(...)`, then auto-respawns and re-primes. A crash-loop guard
  suspends auto-respawn after 3 unexpected exits within 30 seconds; the next IPC
  call attempts one more spawn before failing fast (so the user can recover by
  retrying after fixing the underlying issue).
- **Per-request timeout** (360s) returns `Err(timeout)` to the renderer but does
  **not** kill the still-warm child — the next request will reuse it. The late
  reply from the timed-out turn is dropped by the parser (`pending` is null).
- **Context bytes:** the session tracks a running UTF-8 byte count covering the
  system prompt, every stdin user-turn body, and every stdout assistant content
  body. Reset on respawn. Surfaced as `RequestUsage.contextBytes` for the
  per-request log.

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
  This is where `claudeAgent.test.ts` lives.
- `tests/*.spec.ts` — Playwright end-to-end tests that launch the packaged
  Electron binary from `dist/ide/` and drive the app from a real user's
  perspective (login → dashboard → project → graph editor). `electronTest.ts`
  extends Playwright's `test` fixture to spawn Electron and exposes helpers like
  `loginAsTestUser`, `createNewProject`, `openComponentBrowser`. See
  `tests/README.md` for prerequisites (a built `dist/ide/`, credentials at
  `playwright/.auth/user.json`). Run with
  `corepack pnpm -r --filter enso ide-integration-test [path.spec.ts]`. Runs are
  long (minutes), so avoid them in inner dev loops.

When adding a Playwright test that needs an external dependency the CI doesn't
have yet (e.g. `aiNode.spec.ts` needs the local `claude` CLI), gate the whole
describe block on an env flag (`process.env.ENSO_TEST_AI === '1'`) and note the
flag in the plan's verification section so per-step smokes still exercise it
locally.

`tests/aiChallengePrep.spec.ts` is the heavy AI suite — it drives full Preppin'
Data challenge solves through `AI:` prompts. It's gated on
`ENSO_TEST_AI_CHALLENGES_DIR=/abs/path` pointing at manually-downloaded inputs
(see `tests/README.md` for the expected layout) because the inputs aren't
checked in and the agent budget is real.
