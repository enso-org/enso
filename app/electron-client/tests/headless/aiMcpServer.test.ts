/** @file Unit tests for the in-process MCP server in aiMcpServer.ts. */
import type { AiToolCallReply, AiToolCallRequest } from 'enso-common/src/ai'
import { EventEmitter } from 'node:events'
import * as fs from 'node:fs'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

const ipcEmitter = new EventEmitter()
const ipcOn = vi.fn((channel: string, listener: (...args: unknown[]) => void) =>
  ipcEmitter.on(channel, listener),
)
const ipcRemoveListener = vi.fn((channel: string, listener: (...args: unknown[]) => void) =>
  ipcEmitter.off(channel, listener),
)

vi.mock('electron', () => ({
  ipcMain: { handle: vi.fn(), on: ipcOn, removeListener: ipcRemoveListener },
}))

const { AiMcpServer } = await import('../../src/ai/aiMcpServer')

interface FakeSender {
  isDestroyed: () => boolean
  send: ReturnType<typeof vi.fn>
}

function fakeSender(opts: { destroyed?: boolean } = {}): FakeSender {
  return {
    isDestroyed: () => opts.destroyed === true,
    send: vi.fn(),
  }
}

function fakeActiveRequest(sender: FakeSender, requestId = 'test-request') {
  return { requestId, sender: sender as unknown as Electron.WebContents }
}

/** Reach into the MCP server's private dispatch path to test it without an HTTP client. */
function dispatch(
  server: InstanceType<typeof AiMcpServer>,
  payload: { tool: 'evaluateExpression'; expression: string },
): Promise<AiToolCallReply['result']> {
  // Cast through unknown — tests only.
  const dispatchToRenderer = (
    server as unknown as {
      dispatchToRenderer(payload: unknown): Promise<AiToolCallReply['result']>
    }
  ).dispatchToRenderer.bind(server)
  return dispatchToRenderer(payload)
}

/** Pull the most recent send call off the fake sender — that's the in-flight tool call. */
function lastDispatchedRequest(sender: FakeSender): AiToolCallRequest {
  const calls = sender.send.mock.calls
  expect(calls.length).toBeGreaterThan(0)
  return calls[calls.length - 1]![1] as AiToolCallRequest
}

function answer(request: AiToolCallRequest, result: AiToolCallReply['result']): void {
  ipcEmitter.emit('ai-tool-reply', undefined, { requestId: request.requestId, result })
}

describe('AiMcpServer', () => {
  beforeEach(() => {
    ipcOn.mockClear()
    ipcRemoveListener.mockClear()
  })
  afterEach(() => {
    vi.useRealTimers()
    ipcEmitter.removeAllListeners()
  })

  test('round-trips a renderer reply', async () => {
    const sender = fakeSender()
    const server = new AiMcpServer(() => fakeActiveRequest(sender, 'ai-req-1'))
    const promise = dispatch(server, { tool: 'evaluateExpression', expression: 'x.column_names' })
    // Microtask queue: let dispatch reach the sender.
    await Promise.resolve()
    const request = lastDispatchedRequest(sender)
    expect(request.tool).toBe('evaluateExpression')
    expect(request.expression).toBe('x.column_names')
    expect(request.turnRequestId).toBe('ai-req-1')
    answer(request, { ok: true, value: '["a","b"]' })
    expect(await promise).toEqual({ ok: true, value: '["a","b"]' })
  })

  test('returns "no active AI turn" when the sender resolver yields null', async () => {
    const server = new AiMcpServer(() => null)
    const reply = await dispatch(server, { tool: 'evaluateExpression', expression: 'x' })
    expect(reply).toEqual({ ok: false, error: expect.stringMatching(/no active AI turn/) })
  })

  test('returns "renderer destroyed" when the sender is already gone', async () => {
    const sender = fakeSender({ destroyed: true })
    const server = new AiMcpServer(() => fakeActiveRequest(sender))
    const reply = await dispatch(server, { tool: 'evaluateExpression', expression: 'x' })
    expect(reply).toEqual({ ok: false, error: expect.stringMatching(/destroyed/) })
    // The sender should not have been used to send anything.
    expect(sender.send).not.toHaveBeenCalled()
  })

  test('per-call 30s timeout fires when the renderer never replies', async () => {
    vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] })
    const sender = fakeSender()
    const server = new AiMcpServer(() => fakeActiveRequest(sender))
    const promise = dispatch(server, { tool: 'evaluateExpression', expression: 'x' })
    await Promise.resolve()
    expect(sender.send).toHaveBeenCalled()
    await vi.advanceTimersByTimeAsync(31_000)
    const reply = await promise
    expect(reply.ok).toBe(false)
    if (!reply.ok) expect(reply.error).toMatch(/timed out after 30000ms/)
  })

  test('start writes the temp config file; shutdown removes it', async () => {
    const server = new AiMcpServer(() => null)
    const configPath = await server.start()
    expect(fs.existsSync(configPath)).toBe(true)
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'))
    expect(config.mcpServers.enso.type).toBe('http')
    expect(config.mcpServers.enso.url).toMatch(/^http:\/\/127\.0\.0\.1:\d+\/mcp$/)
    await server.shutdown()
    expect(fs.existsSync(configPath)).toBe(false)
  })

  test('shutdown rejects in-flight dispatches with a clean error', async () => {
    const sender = fakeSender()
    const server = new AiMcpServer(() => fakeActiveRequest(sender))
    const promise = dispatch(server, { tool: 'evaluateExpression', expression: 'x' })
    await Promise.resolve()
    await server.shutdown()
    const reply = await promise
    expect(reply.ok).toBe(false)
    if (!reply.ok) expect(reply.error).toMatch(/shutting down/)
  })

  test('stale replies (after timeout/shutdown) are dropped silently', async () => {
    const sender = fakeSender()
    const server = new AiMcpServer(() => fakeActiveRequest(sender))
    // Manually create a request that has already been resolved by shutdown.
    const promise = dispatch(server, { tool: 'evaluateExpression', expression: 'x' })
    await Promise.resolve()
    const request = lastDispatchedRequest(sender)
    await server.shutdown()
    await promise
    // Late reply must not throw.
    expect(() => answer(request, { ok: true, value: '42' })).not.toThrow()
  })
})
