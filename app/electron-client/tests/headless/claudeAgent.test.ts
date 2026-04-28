/** @file Unit tests for the `claude` CLI shell-out in claudeAgent.ts. */
import type { AiComponentRequest, AiComponentResponse } from 'enso-common/src/ai'
import { EventEmitter } from 'node:events'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

const { spawnMock } = vi.hoisted(() => ({ spawnMock: vi.fn() }))

vi.mock('cross-spawn', () => ({ default: spawnMock }))
vi.mock('electron', () => ({ ipcMain: { handle: vi.fn() } }))

const { generateAiComponent } = await import('../../src/claudeAgent')

interface FakeChildOptions {
  stdout?: string
  stderr?: string
  exitCode?: number | null
  spawnError?: NodeJS.ErrnoException
}

// The real ChildProcess surface is large; the production code only uses a small subset,
// so the fake declares just that subset and we cast to `unknown` when handing it back.
interface FakeChild {
  stdout: EventEmitter
  stderr: EventEmitter
  stdin: { end: ReturnType<typeof vi.fn>; on: ReturnType<typeof vi.fn> }
  kill: ReturnType<typeof vi.fn>
  exitCode: number | null
  on(event: 'error', cb: (err: Error) => void): FakeChild
  on(event: 'close', cb: (code: number | null) => void): FakeChild
  emit(event: string, ...args: unknown[]): boolean
}

function makeFakeChild(opts: FakeChildOptions = {}): FakeChild {
  const emitter = new EventEmitter()
  const stdout = new EventEmitter()
  const stderr = new EventEmitter()
  const child = Object.assign(emitter, {
    stdout,
    stderr,
    stdin: { end: vi.fn(), on: vi.fn() },
    kill: vi.fn(() => true),
    exitCode: opts.exitCode ?? 0,
  }) as unknown as FakeChild
  queueMicrotask(() => {
    if (opts.stdout) stdout.emit('data', Buffer.from(opts.stdout))
    if (opts.stderr) stderr.emit('data', Buffer.from(opts.stderr))
    if (opts.spawnError) child.emit('error', opts.spawnError)
    queueMicrotask(() => child.emit('close', opts.exitCode ?? 0))
  })
  return child
}

function envelopeWith(structuredOutput: unknown): string {
  return JSON.stringify({
    type: 'result',
    subtype: 'success',
    result: '',
    // eslint-disable-next-line camelcase
    structured_output: structuredOutput,
  })
}

const exampleRequest: AiComponentRequest = {
  prompt: 'filter to rows with value over 5',
  context: {
    sourceIdentifier: 'source',
    sourceTypeName: 'Standard.Table.Table',
    currentMethodName: 'main',
    currentMethodCode: 'main =\n    source = Table.new []\n    source',
    inScopeBindings: [{ identifier: 'helper', typeName: 'Standard.Base.Number' }],
  },
}

const exampleResponse: AiComponentResponse = {
  functionName: 'filter_rows',
  argumentNames: ['source'],
  body: 'filtered = source.filter (row -> row.value > 5)\nfiltered',
  callArguments: ['source'],
}

describe('generateAiComponent', () => {
  beforeEach(() => {
    spawnMock.mockReset()
  })
  afterEach(() => {
    vi.clearAllMocks()
  })

  test('returns the parsed response on a successful CLI invocation', async () => {
    spawnMock.mockReturnValue(makeFakeChild({ stdout: envelopeWith(exampleResponse) }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(true)
    if (result.ok) expect(result.value).toEqual(exampleResponse)
    const [executable, args] = spawnMock.mock.calls[0]!
    expect(executable).toBe('claude')
    expect(args).toContain('--print')
    expect(args).toContain('--output-format')
    expect(args).toContain('--json-schema')
    expect(args).toContain('--system-prompt')
    expect(args).toContain('--tools')
    expect(args).toContain('--no-session-persistence')
  })

  test('writes the prompt and context to stdin and closes it', async () => {
    const fake = makeFakeChild({ stdout: envelopeWith(exampleResponse) })
    spawnMock.mockReturnValue(fake)
    await generateAiComponent(exampleRequest)
    expect(fake.stdin.end).toHaveBeenCalledOnce()
    const [payload] = fake.stdin.end.mock.calls[0]!
    expect(payload).toContain('filter to rows with value over 5')
    expect(payload).toContain('source')
    expect(payload).toContain('Standard.Table.Table')
    expect(payload).toContain('Current method: main')
    expect(payload).toContain('helper')
  })

  test('returns an ENOENT-specific error when claude is not on PATH', async () => {
    const error = Object.assign(new Error('spawn claude ENOENT'), { code: 'ENOENT' })
    spawnMock.mockReturnValue(makeFakeChild({ spawnError: error, exitCode: null }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(false)
    if (!result.ok) expect(result.error.payload).toMatch(/not found on PATH/)
  })

  test('returns a generic spawn error for non-ENOENT spawn failures', async () => {
    const error = Object.assign(new Error('spawn claude EACCES'), { code: 'EACCES' })
    spawnMock.mockReturnValue(makeFakeChild({ spawnError: error, exitCode: null }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(false)
    if (!result.ok) {
      expect(result.error.payload).toMatch(/Failed to spawn 'claude'/)
      expect(result.error.payload).toMatch(/EACCES/)
    }
  })

  test('returns an error on non-zero exit and includes the stderr tail', async () => {
    spawnMock.mockReturnValue(
      makeFakeChild({ stdout: '', stderr: 'authentication failure', exitCode: 1 }),
    )
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(false)
    if (!result.ok) {
      expect(result.error.payload).toMatch(/exited with code 1/)
      expect(result.error.payload).toMatch(/authentication failure/)
    }
  })

  test('returns an error when stdout is not valid JSON', async () => {
    spawnMock.mockReturnValue(makeFakeChild({ stdout: 'not json' }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(false)
    if (!result.ok) expect(result.error.payload).toMatch(/malformed JSON/)
  })

  test('returns an error when the payload is missing required fields', async () => {
    spawnMock.mockReturnValue(makeFakeChild({ stdout: envelopeWith({ body: 'ok' }) }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(false)
    if (!result.ok) expect(result.error.payload).toMatch(/did not match the expected response schema/)
  })

  test('falls back to the `result` field when `structured_output` is absent', async () => {
    const legacyPayload: AiComponentResponse = {
      functionName: 'legacy_fn',
      argumentNames: ['source'],
      body: 'source',
      callArguments: ['source'],
    }
    const envelope = JSON.stringify({
      type: 'result',
      subtype: 'success',
      result: JSON.stringify(legacyPayload),
    })
    spawnMock.mockReturnValue(makeFakeChild({ stdout: envelope }))
    const result = await generateAiComponent(exampleRequest)
    expect(result.ok).toBe(true)
    if (result.ok) expect(result.value).toEqual(legacyPayload)
  })
})
