/** @file Unit tests for the long-lived `claude` session in claudeAgent.ts. */
import type { AiComponentRequest, AiComponentResponse } from 'enso-common/src/ai'
import { EventEmitter } from 'node:events'
import { Readable, Writable } from 'node:stream'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

const { spawnMock } = vi.hoisted(() => ({ spawnMock: vi.fn() }))

vi.mock('cross-spawn', () => ({ default: spawnMock }))
vi.mock('electron', () => ({ ipcMain: { handle: vi.fn(), on: vi.fn() } }))

/** A `WebContents`-shaped stub good enough for the constructor + activeSender invariants. */
function fakeSender(opts: { destroyed?: boolean } = {}) {
  return {
    isDestroyed: () => opts.destroyed === true,
    send: vi.fn(),
  } as unknown as Electron.WebContents
}

const { ClaudeAgentSession } = await import('../../src/claudeAgent')

class FakeChild extends EventEmitter {
  exitCode: number | null = null
  killCalls: NodeJS.Signals[] = []
  stdinWrites: string[] = []
  stdout = new Readable({ read() {} })
  stderr = new Readable({ read() {} })
  stdin: Writable & { destroyed: boolean }

  constructor() {
    super()
    const writes = this.stdinWrites
    const stream: Writable & { destroyed?: boolean } = new Writable({
      write(chunk: Buffer, _enc: BufferEncoding, callback: (err?: Error | null) => void) {
        writes.push(chunk.toString('utf8'))
        callback()
      },
    })
    stream.destroyed = false
    this.stdin = stream as Writable & { destroyed: boolean }
  }

  pushStdoutLine(line: string): void {
    this.stdout.push(line + '\n')
  }
  pushStderr(text: string): void {
    this.stderr.push(text)
  }
  crash(code: number, stderr?: string): void {
    if (stderr != null) this.stderr.push(stderr)
    this.exitCode = code
    queueMicrotask(() => this.emit('exit', code, null))
  }
  kill(signal: NodeJS.Signals = 'SIGTERM'): boolean {
    this.killCalls.push(signal)
    this.exitCode = signal === 'SIGTERM' ? 143 : 137
    queueMicrotask(() => this.emit('exit', this.exitCode, signal))
    return true
  }
}

const exampleRequest: AiComponentRequest = {
  requestId: 'req-test',
  prompt: 'filter to rows with value over 5',
  context: {
    sourceIdentifier: 'source',
    sourceTypeName: 'Standard.Table.Table',
    currentMethodName: 'main',
    currentMethodCode: 'main =\n    source = Table.new []\n    source',
    inScopeBindings: [{ identifier: 'helper', typeName: 'Standard.Base.Number' }],
    moduleImports: ['from Standard.Base import all', 'from Standard.Table import all'],
  },
}

/** Build a request with a unique id, defaulting to {@link exampleRequest}'s shape. */
function makeRequest(overrides: Partial<AiComponentRequest> = {}): AiComponentRequest {
  return {
    ...exampleRequest,
    requestId: overrides.requestId ?? `req-${Math.random().toString(36).slice(2)}`,
    ...overrides,
  }
}

const exampleResponse: AiComponentResponse = {
  functionName: 'filter_rows',
  argumentNames: ['source'],
  body: 'filtered = source.filter (row -> row.value > 5)\nfiltered',
  callArguments: ['source'],
}

/* eslint-disable camelcase -- mirrors the snake_case keys the real CLI emits in stream-json. */
const exampleUsage = {
  input_tokens: 1234,
  output_tokens: 56,
  cache_read_input_tokens: 5000,
  cache_creation_input_tokens: 2000,
}

function resultEnvelope(textOrObject: unknown, usage = exampleUsage): string {
  const result = typeof textOrObject === 'string' ? textOrObject : JSON.stringify(textOrObject)
  return JSON.stringify({
    type: 'result',
    subtype: 'success',
    is_error: false,
    terminal_reason: 'completed',
    result,
    usage,
  })
}

/**
 * Build a synthetic `assistant` envelope. When `usage` is non-null, it lands on `message.usage`
 * — the same shape Anthropic's Messages API returns and the CLI passes through, used here to
 * exercise the per-hop `contextTokens` capture path in `captureAssistantContent`.
 */
function assistantEnvelope(text: string, usage: typeof exampleUsage | null = null): string {
  const message: Record<string, unknown> = { content: [{ type: 'text', text }] }
  if (usage != null) message.usage = usage
  return JSON.stringify({ type: 'assistant', message })
}
/* eslint-enable camelcase */

function readyEnvelope(): string {
  return resultEnvelope('READY')
}

/** Wait one macrotask (lets queued microtasks resolve). */
function tick(): Promise<void> {
  return new Promise((resolve) => setImmediate(resolve))
}

async function settle(): Promise<void> {
  for (let i = 0; i < 5; i++) await tick()
}

interface SessionHarness {
  session: InstanceType<typeof ClaudeAgentSession>
  children: FakeChild[]
}

const FAKE_STDLIB_ROOT = '/fake/engine-bundle/lib/Standard'

function buildSession(
  config: { stdlibRoot?: string | undefined; mcpConfigPath?: string | undefined } = {},
): SessionHarness {
  const children: FakeChild[] = []
  spawnMock.mockImplementation(() => {
    const child = new FakeChild()
    children.push(child)
    return child as unknown as ReturnType<typeof import('node:child_process').spawn>
  })
  const session = new ClaudeAgentSession({
    stdlibRoot: 'stdlibRoot' in config ? config.stdlibRoot : FAKE_STDLIB_ROOT,
    mcpConfigPath: 'mcpConfigPath' in config ? config.mcpConfigPath : undefined,
  })
  return { session, children }
}

async function primeChild(child: FakeChild): Promise<void> {
  await settle()
  // The session's prime() should have written the priming user turn by now.
  child.pushStdoutLine(readyEnvelope())
  await settle()
}

describe('ClaudeAgentSession', () => {
  beforeEach(() => {
    spawnMock.mockReset()
  })
  afterEach(() => {
    vi.useRealTimers()
    vi.clearAllMocks()
  })

  test('priming completes; first request succeeds with usage', async () => {
    const { session, children } = buildSession()
    expect(spawnMock).toHaveBeenCalledTimes(1)
    const [executable, args] = spawnMock.mock.calls[0]!
    expect(executable).toBe('claude')
    expect(args).toContain('--input-format')
    expect(args).toContain('stream-json')
    expect(args).toContain('--no-session-persistence')
    // The deprecated `--tools` flag must not appear; allow-listing is done via `--allowedTools`.
    expect(args).not.toContain('--tools')
    // With stdlib root provided, expect `--add-dir <root>` and `--allowedTools Read,Glob,Grep`.
    const addDirIdx = args.indexOf('--add-dir')
    expect(addDirIdx).toBeGreaterThan(-1)
    expect(args[addDirIdx + 1]).toBe(FAKE_STDLIB_ROOT)
    const allowedToolsIdx = args.indexOf('--allowedTools')
    expect(allowedToolsIdx).toBeGreaterThan(-1)
    expect(args[allowedToolsIdx + 1]).toBe('Read,Glob,Grep')

    await primeChild(children[0]!)
    expect(children[0]!.stdinWrites).toHaveLength(1)
    expect(children[0]!.stdinWrites[0]).toContain('Acknowledge readiness')

    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(2)
    expect(children[0]!.stdinWrites[1]).toContain('filter to rows with value over 5')

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const reply = await replyPromise
    expect(reply.result.ok).toBe(true)
    if (reply.result.ok) expect(reply.result.value).toEqual(exampleResponse)
    expect(reply.usage).not.toBeNull()
    expect(reply.usage!.inputTokens).toBe(exampleUsage.input_tokens)
    expect(reply.usage!.outputTokens).toBe(exampleUsage.output_tokens)
    expect(reply.usage!.cacheReadTokens).toBe(exampleUsage.cache_read_input_tokens)
    expect(reply.usage!.cacheCreationTokens).toBe(exampleUsage.cache_creation_input_tokens)
    // No `assistant` envelope was pushed in this test, so `lastHopUsage` is null and
    // `contextTokens` falls back to the `result.usage` sum (= the prior behavior). The flag
    // surfaces the fallback to consumers; with `hopCount == 0` it's the degenerate "no model
    // output observed" case rather than the broken "tool turn missed final usage" case.
    expect(reply.usage!.contextTokens).toBe(
      exampleUsage.input_tokens +
        exampleUsage.cache_read_input_tokens +
        exampleUsage.cache_creation_input_tokens,
    )
    expect(reply.usage!.contextFromLastHop).toBe(false)
    expect(reply.usage!.hopCount).toBe(0)
    session.shutdown()
  })

  test('contextTokens comes from the LAST assistant envelope, not result.usage', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)

    /* eslint-disable camelcase */
    const earlyHop = {
      input_tokens: 10,
      output_tokens: 20,
      cache_read_input_tokens: 1000,
      cache_creation_input_tokens: 500,
    }
    const finalHop = {
      input_tokens: 30,
      output_tokens: 40,
      cache_read_input_tokens: 70_000,
      cache_creation_input_tokens: 2_000,
    }
    // Result usage is the cost-side roll-up (CLI sums across hops); deliberately distinct from
    // either per-hop value so the assertions can tell them apart.
    const resultUsage = {
      input_tokens: 999,
      output_tokens: 888,
      cache_read_input_tokens: 77_777,
      cache_creation_input_tokens: 6_666,
    }
    /* eslint-enable camelcase */

    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.pushStdoutLine(assistantEnvelope('thinking…', earlyHop))
    children[0]!.pushStdoutLine(assistantEnvelope(JSON.stringify(exampleResponse), finalHop))
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, resultUsage))
    const reply = await replyPromise

    expect(reply.usage).not.toBeNull()
    // Cost-side fields come from the result envelope.
    expect(reply.usage!.inputTokens).toBe(resultUsage.input_tokens)
    expect(reply.usage!.outputTokens).toBe(resultUsage.output_tokens)
    expect(reply.usage!.cacheReadTokens).toBe(resultUsage.cache_read_input_tokens)
    expect(reply.usage!.cacheCreationTokens).toBe(resultUsage.cache_creation_input_tokens)
    // contextTokens uses the LAST assistant envelope's usage (the synthesis call's actual
    // prompt size), not the cost-side sum.
    expect(reply.usage!.contextTokens).toBe(
      finalHop.input_tokens +
        finalHop.cache_read_input_tokens +
        finalHop.cache_creation_input_tokens,
    )
    expect(reply.usage!.contextFromLastHop).toBe(true)
    expect(reply.usage!.hopCount).toBe(2)
    session.shutdown()
  })

  test('contextFromLastHop is false when the final assistant envelope omits usage', async () => {
    // Captures the broken case the user is concerned about: an early hop carries `usage`,
    // but the final synthesis envelope (post-tool) omits it. We must NOT keep the stale
    // earlier value — `lastHopUsage` is overwritten with `null` on every envelope, so the
    // flag flips to false and consumers (the metrics writer) can refuse to record the row.
    const { session, children } = buildSession()
    await primeChild(children[0]!)

    /* eslint-disable camelcase */
    const earlyHop = {
      input_tokens: 5,
      output_tokens: 10,
      cache_read_input_tokens: 100,
      cache_creation_input_tokens: 0,
    }
    const resultUsage = {
      input_tokens: 99,
      output_tokens: 99,
      cache_read_input_tokens: 9999,
      cache_creation_input_tokens: 999,
    }
    /* eslint-enable camelcase */

    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.pushStdoutLine(assistantEnvelope('thinking…', earlyHop))
    // Final envelope: no `usage` — simulates the broken case.
    children[0]!.pushStdoutLine(assistantEnvelope(JSON.stringify(exampleResponse)))
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, resultUsage))
    const reply = await replyPromise

    expect(reply.usage).not.toBeNull()
    expect(reply.usage!.contextFromLastHop).toBe(false)
    expect(reply.usage!.hopCount).toBe(2)
    // contextTokens fell back to the result-envelope sum (NOT `earlyHop`'s — the early hop's
    // value would be stale and misleading).
    expect(reply.usage!.contextTokens).toBe(
      resultUsage.input_tokens +
        resultUsage.cache_read_input_tokens +
        resultUsage.cache_creation_input_tokens,
    )
    session.shutdown()
  })

  test('two consecutive requests reuse the same child', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)

    const r1 = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await r1

    const r2 = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await r2

    expect(spawnMock).toHaveBeenCalledTimes(1)
    expect(children).toHaveLength(1)
    session.shutdown()
  })

  test('two overlapping requests are serialized', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    expect(children[0]!.stdinWrites).toHaveLength(1)

    const r1 = session.runRequest(exampleRequest, fakeSender())
    const r2 = session.runRequest(exampleRequest, fakeSender())
    await settle()
    // Only the first request should have written to stdin so far; the second waits in the queue.
    expect(children[0]!.stdinWrites).toHaveLength(2)

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await r1
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(3)

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await r2
    session.shutdown()
  })

  test('crash mid-request fails the request and respawns', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)

    const r1 = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.crash(1, 'authentication failure')
    const reply1 = await r1
    expect(reply1.result.ok).toBe(false)
    if (!reply1.result.ok) expect(reply1.result.error.payload).toMatch(/exited with code=1/)

    // After the crash, spawn should have been called again automatically.
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(2)
    expect(children).toHaveLength(2)
    await primeChild(children[1]!)

    const r2 = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const reply2 = await r2
    expect(reply2.result.ok).toBe(true)
    session.shutdown()
  })

  test('crash-loop guard suspends respawn after three failures', async () => {
    const { session, children } = buildSession()
    // Crash three times in succession during priming (before READY is pushed).
    for (let i = 0; i < 3; i++) {
      await settle()
      children[children.length - 1]!.crash(1, 'boom')
      await settle()
    }
    // After the 3rd crash, respawn should have been suspended; spawn count caps at 3.
    expect(spawnMock).toHaveBeenCalledTimes(3)

    // The next request should attempt one more spawn (the "next IPC call retries once" rule),
    // and when that crashes, we get a clean Err — no infinite respawn.
    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(4)
    children[3]!.crash(1, 'still broken')
    const reply = await replyPromise
    expect(reply.result.ok).toBe(false)
    session.shutdown()
  })

  test('per-request timeout returns Err without killing the child', async () => {
    vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] })
    const { session, children } = buildSession()
    // Use a tick-only flush; primeChild can't await real-time settle when fake timers are on.
    children[0]!.pushStdoutLine(readyEnvelope())
    await Promise.resolve()
    await Promise.resolve()
    await Promise.resolve()
    await Promise.resolve()

    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await Promise.resolve()
    await Promise.resolve()
    await Promise.resolve()
    expect(children[0]!.stdinWrites.length).toBeGreaterThanOrEqual(1)

    // Advance past the per-request timeout (REQUEST_TIMEOUT_MS in claudeAgent.ts).
    await vi.advanceTimersByTimeAsync(361_000)
    const reply = await replyPromise
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok) expect(reply.result.error.payload).toMatch(/timed out/)

    // Child should NOT have been killed by the timeout.
    expect(children[0]!.killCalls).toHaveLength(0)
    expect(spawnMock).toHaveBeenCalledTimes(1)

    session.shutdown()
  })

  test('omits --add-dir / --allowedTools when stdlib root cannot be resolved', () => {
    const { session } = buildSession({ stdlibRoot: undefined })
    expect(spawnMock).toHaveBeenCalledTimes(1)
    const [, args] = spawnMock.mock.calls[0]!
    expect(args).not.toContain('--add-dir')
    expect(args).not.toContain('--allowedTools')
    // The agent still spawns; it just runs without filesystem context.
    expect(args).toContain('--input-format')
    session.shutdown()
  })

  test('with mcpConfigPath, --mcp-config + --strict-mcp-config + tool allow-list are wired', () => {
    const { session } = buildSession({ mcpConfigPath: '/tmp/fake-mcp.json' })
    expect(spawnMock).toHaveBeenCalledTimes(1)
    const [, args] = spawnMock.mock.calls[0]!
    const mcpIdx = args.indexOf('--mcp-config')
    expect(mcpIdx).toBeGreaterThan(-1)
    expect(args[mcpIdx + 1]).toBe('/tmp/fake-mcp.json')
    expect(args).toContain('--strict-mcp-config')
    const allowedToolsIdx = args.indexOf('--allowedTools')
    expect(allowedToolsIdx).toBeGreaterThan(-1)
    // With both stdlib + MCP enabled, all three filesystem tools plus the MCP tool show up.
    expect(args[allowedToolsIdx + 1]).toBe('Read,Glob,Grep,mcp__enso__evaluateExpression')
    session.shutdown()
  })

  test('runRequest rejects early when sender is already destroyed', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const reply = await session.runRequest(exampleRequest, fakeSender({ destroyed: true }))
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok)
      expect(reply.result.error.payload).toMatch(/destroyed before the AI request/)
    // No stdin write should have happened — the request was rejected before reaching the queue.
    expect(children[0]!.stdinWrites).toHaveLength(1) // priming only
    session.shutdown()
  })

  test('activeRequest returns null between turns and the live sender + id mid-turn', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    expect(session.activeRequest).toBeNull()

    const sender = fakeSender()
    const request = makeRequest()
    const replyPromise = session.runRequest(request, sender)
    await settle()
    expect(session.activeRequest).toEqual({ requestId: request.requestId, sender })

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await replyPromise
    expect(session.activeRequest).toBeNull()
    session.shutdown()
  })

  test('activeRequest returns null when the in-flight sender has been destroyed mid-turn', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    let destroyed = false
    const sender = {
      isDestroyed: () => destroyed,
      send: vi.fn(),
    } as unknown as Electron.WebContents

    const request = makeRequest()
    const replyPromise = session.runRequest(request, sender)
    await settle()
    expect(session.activeRequest).toEqual({ requestId: request.requestId, sender })

    destroyed = true
    expect(session.activeRequest).toBeNull()

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await replyPromise
    session.shutdown()
  })

  test('shutdown() sends SIGTERM and rejects in-flight work', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()

    session.shutdown()
    const reply = await replyPromise
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok) expect(reply.result.error.payload).toMatch(/shutting down/)
    expect(children[0]!.killCalls).toContain('SIGTERM')
  })

  test('emits ai-progress: queued + started + text on a successful turn', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const sender = fakeSender()
    // The send mock on `sender` is the one to inspect. Cast back to access it.
    const sendMock = (sender as unknown as { send: ReturnType<typeof vi.fn> }).send
    const request = makeRequest({ requestId: 'req-progress' })
    const replyPromise = session.runRequest(request, sender)
    await settle()
    // `queued` should fire on IPC receipt (before any awaits in the queue task), and `started`
    // once the turn actually begins.
    const queuedCalls = sendMock.mock.calls.filter(
      (c) => c[0] === 'ai-progress' && c[1].kind === 'queued',
    )
    expect(queuedCalls).toHaveLength(1)
    expect(queuedCalls[0]![1]).toEqual({ requestId: 'req-progress', kind: 'queued' })
    const startedCalls = sendMock.mock.calls.filter(
      (c) => c[0] === 'ai-progress' && c[1].kind === 'started',
    )
    expect(startedCalls).toHaveLength(1)
    expect(startedCalls[0]![1]).toEqual({ requestId: 'req-progress', kind: 'started' })
    // The two events arrive in order: `queued` before `started`.
    const orderedKinds = sendMock.mock.calls
      .filter((c) => c[0] === 'ai-progress')
      .map((c) => c[1].kind)
    const queuedIdx = orderedKinds.indexOf('queued')
    const startedIdx = orderedKinds.indexOf('started')
    expect(queuedIdx).toBeGreaterThanOrEqual(0)
    expect(startedIdx).toBeGreaterThan(queuedIdx)

    // Synthesize an assistant envelope with a text block — captureAssistantContent should fire
    // a `text` progress event.
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'assistant',
        message: { content: [{ type: 'text', text: 'Reading the table…' }] },
      }),
    )
    await settle()
    const textCalls = sendMock.mock.calls.filter(
      (c) => c[0] === 'ai-progress' && c[1].kind === 'text',
    )
    expect(textCalls).toHaveLength(1)
    expect(textCalls[0]![1]).toEqual({
      requestId: 'req-progress',
      kind: 'text',
      text: 'Reading the table…',
    })

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await replyPromise
    session.shutdown()
  })

  test('emits ai-progress: tool with raw input for built-in and MCP tool_use blocks', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const sender = fakeSender()
    const sendMock = (sender as unknown as { send: ReturnType<typeof vi.fn> }).send
    const request = makeRequest({ requestId: 'req-tool' })
    const replyPromise = session.runRequest(request, sender)
    await settle()

    /* eslint-disable camelcase -- mirrors the snake_case `file_path` key the real CLI emits. */
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'assistant',
        message: {
          content: [
            {
              type: 'tool_use',
              name: 'Read',
              input: { file_path: '/lib/Standard/Table/0.0.0/Main.enso' },
            },
          ],
        },
      }),
    )
    /* eslint-enable camelcase */
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'assistant',
        message: {
          content: [
            {
              type: 'tool_use',
              name: 'mcp__enso__evaluateExpression',
              input: { expression: 'source.column_names.to_json' },
            },
          ],
        },
      }),
    )
    await settle()
    const toolCalls = sendMock.mock.calls.filter(
      (c) => c[0] === 'ai-progress' && c[1].kind === 'tool',
    )
    expect(toolCalls).toHaveLength(2)
    expect(toolCalls[0]![1]).toEqual({
      requestId: 'req-tool',
      kind: 'tool',
      toolName: 'Read',
      // eslint-disable-next-line camelcase -- the raw input is forwarded as-is, snake_case included.
      input: { file_path: '/lib/Standard/Table/0.0.0/Main.enso' },
    })
    expect(toolCalls[1]![1]).toEqual({
      requestId: 'req-tool',
      kind: 'tool',
      toolName: 'mcp__enso__evaluateExpression',
      input: { expression: 'source.column_names.to_json' },
    })

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await replyPromise
    session.shutdown()
  })

  test('cancelTurn on the in-flight request resolves with cancellation Err and SIGINTs the child', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const sender = fakeSender()
    const request = makeRequest({ requestId: 'req-cancel' })
    const replyPromise = session.runRequest(request, sender)
    await settle()
    // Mid-turn: the renderer cancels.
    session.cancelTurn(request.requestId)
    const reply = await replyPromise
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok) expect(reply.result.error.payload).toMatch(/cancelled by user/)
    // SIGINT should have been sent to the live child (FakeChild treats kill as terminal — the
    // 2-second SIGTERM watchdog never fires because the child has already exited via SIGINT).
    expect(children[0]!.killCalls[0]).toBe('SIGINT')
    session.shutdown()
  })

  test('cancelTurn for an id that was never issued does not poison a later request reusing that id', async () => {
    // Regression guard: previously, `cancelTurn` always added to the `cancelled` set. A cancel
    // arriving for an id that hadn't been (or would never be) enqueued would sit there forever,
    // and a future `runRequest` reusing the same id would short-circuit on entry. Tightened so
    // unknown ids are dropped.
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    session.cancelTurn('stranger-id')
    const reply = session.runRequest(makeRequest({ requestId: 'stranger-id' }), fakeSender())
    await settle()
    // Priming + the new request — the request was NOT short-circuited.
    expect(children[0]!.stdinWrites).toHaveLength(2)
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const settled = await reply
    expect(settled.result.ok).toBe(true)
    session.shutdown()
  })

  test('cancelTurn on a queued request short-circuits without writing stdin', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    const sender1 = fakeSender()
    const sender2 = fakeSender()
    const request1 = makeRequest({ requestId: 'req-q1' })
    const request2 = makeRequest({ requestId: 'req-q2' })
    const r1 = session.runRequest(request1, sender1)
    const r2 = session.runRequest(request2, sender2)
    await settle()
    // Only request1 has reached `runOneTurn` so far.
    expect(children[0]!.stdinWrites).toHaveLength(2) // priming + r1
    // Cancel the still-queued r2 before r1 finishes.
    session.cancelTurn(request2.requestId)

    // Finish r1 normally.
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await r1
    // r2 should have short-circuited without ever writing to stdin.
    const r2Reply = await r2
    expect(r2Reply.result.ok).toBe(false)
    if (!r2Reply.result.ok) expect(r2Reply.result.error.payload).toMatch(/Cancelled by user/)
    expect(children[0]!.stdinWrites).toHaveLength(2) // still just priming + r1
    session.shutdown()
  })
})
