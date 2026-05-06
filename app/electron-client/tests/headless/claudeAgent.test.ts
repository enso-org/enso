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
    expect(reply.usage!.contextBytes).toBeGreaterThan(0)
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

  test('activeSender returns null between turns and the live sender mid-turn', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    expect(session.activeSender).toBeNull()

    const sender = fakeSender()
    const replyPromise = session.runRequest(exampleRequest, sender)
    await settle()
    expect(session.activeSender).toBe(sender)

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    await replyPromise
    expect(session.activeSender).toBeNull()
    session.shutdown()
  })

  test('activeSender returns null when the in-flight sender has been destroyed mid-turn', async () => {
    const { session, children } = buildSession()
    await primeChild(children[0]!)
    let destroyed = false
    const sender = {
      isDestroyed: () => destroyed,
      send: vi.fn(),
    } as unknown as Electron.WebContents

    const replyPromise = session.runRequest(exampleRequest, sender)
    await settle()
    expect(session.activeSender).toBe(sender)

    destroyed = true
    expect(session.activeSender).toBeNull()

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
})
