/**
 * @file Shared mocks, fakes, and envelope builders for the claudeAgent / claudeAgentChild
 * unit tests. The `vi.mock('cross-spawn', ...)` and `vi.mock('electron', ...)` calls have to
 * stay in each test file (vitest hoists them per-file), but everything that doesn't need
 * top-level hoisting lives here.
 */
import type { AiComponentRequest, AiComponentResponse } from 'enso-common/src/ai'
import { EventEmitter } from 'node:events'
import { Readable, Writable } from 'node:stream'
import { vi, type MockInstance } from 'vitest'

/**
 * Stand-in for an Electron `WebContents` good enough for the senders the agent code passes
 * around (`isDestroyed()` query and `send(channel, payload)`).
 */
export function fakeSender(opts: { destroyed?: boolean } = {}) {
  return {
    isDestroyed: () => opts.destroyed === true,
    send: vi.fn(),
  } as unknown as Electron.WebContents
}

/**
 * Replays the surface of a `ChildProcess` that the agent code actually touches (stdin write,
 * stdout/stderr lines, kill, exit). Exposes hooks for the test to push lines, force a crash,
 * and inspect what was written.
 */
export class FakeChild extends EventEmitter {
  exitCode: number | null = null
  killCalls: NodeJS.Signals[] = []
  stdinWrites: string[] = []
  stdout = new Readable({ read() {} })
  stderr = new Readable({ read() {} })
  stdin: Writable & { destroyed: boolean }

  /** Wire stdin so writes accumulate in {@link stdinWrites} for assertions. */
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

  /** Push a line into stdout (newline appended) so the readline parser emits it as 'line'. */
  pushStdoutLine(line: string): void {
    this.stdout.push(line + '\n')
  }
  /** Append text to stderr (no newline added). */
  pushStderr(text: string): void {
    this.stderr.push(text)
  }
  /** Simulate a crash: push optional stderr, set exit code, emit 'exit' on next microtask. */
  crash(code: number, stderr?: string): void {
    if (stderr != null) this.stderr.push(stderr)
    this.exitCode = code
    queueMicrotask(() => this.emit('exit', code, null))
  }
  /** Record the kill signal and emit 'exit' on next microtask, mirroring real `child.kill`. */
  kill(signal: NodeJS.Signals = 'SIGTERM'): boolean {
    this.killCalls.push(signal)
    this.exitCode = signal === 'SIGTERM' ? 143 : 137
    queueMicrotask(() => this.emit('exit', this.exitCode, signal))
    return true
  }
}

/**
 * Wire `spawnMock` (the per-file hoisted `vi.fn()` standing in for `cross-spawn`) so each call
 * yields a fresh {@link FakeChild}, and return the array tests can read to find spawned
 * children. Also resets the mock so previously-stored calls do not leak across tests.
 */
export function attachSpawnMock(spawnMock: MockInstance): { children: FakeChild[] } {
  const children: FakeChild[] = []
  spawnMock.mockReset()
  spawnMock.mockImplementation(() => {
    const child = new FakeChild()
    children.push(child)
    return child as unknown as ReturnType<typeof import('node:child_process').spawn>
  })
  return { children }
}

/** Wait one macrotask (lets queued microtasks resolve). */
export function tick(): Promise<void> {
  return new Promise((resolve) => setImmediate(resolve))
}

/** Drain pending microtasks/macrotasks so async I/O reaches steady state. */
export async function settle(): Promise<void> {
  for (let i = 0; i < 5; i++) await tick()
}

/** Push the priming `READY` envelope so the child's priming completes. */
export async function primeChild(child: FakeChild): Promise<void> {
  await settle()
  child.pushStdoutLine(readyEnvelope())
  await settle()
}

export const FAKE_STDLIB_ROOT = '/fake/engine-bundle/lib/Standard'

/* eslint-disable camelcase -- mirrors the snake_case keys the real CLI emits in stream-json. */
export const exampleUsage = {
  input_tokens: 1234,
  output_tokens: 56,
  cache_read_input_tokens: 5000,
  cache_creation_input_tokens: 2000,
}

/** Build a `result` envelope with optional structured payload + usage override. */
export function resultEnvelope(textOrObject: unknown, usage = exampleUsage): string {
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
 * — the same shape Anthropic's Messages API returns and the CLI passes through, used to
 * exercise the per-hop `contextTokens` capture path.
 */
export function assistantEnvelope(text: string, usage: typeof exampleUsage | null = null): string {
  const message: Record<string, unknown> = { content: [{ type: 'text', text }] }
  if (usage != null) message.usage = usage
  return JSON.stringify({ type: 'assistant', message })
}
/* eslint-enable camelcase */

/** Mint a result envelope whose `result` is a single `READY` (the priming reply shape). */
export function readyEnvelope(): string {
  return resultEnvelope('READY')
}

export const exampleRequest: AiComponentRequest = {
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
export function makeRequest(overrides: Partial<AiComponentRequest> = {}): AiComponentRequest {
  return {
    ...exampleRequest,
    requestId: overrides.requestId ?? `req-${Math.random().toString(36).slice(2)}`,
    ...overrides,
  }
}

export const exampleResponse: AiComponentResponse = {
  functionName: 'filter_rows',
  argumentNames: ['source'],
  body: 'filtered = source.filter (row -> row.value > 5)\nfiltered',
  callArguments: ['source'],
}
