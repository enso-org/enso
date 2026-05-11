/** @file Unit tests for one `ChildAgent` instance — priming, turns, stdout parsing, spawn flags, crashes. */
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import {
  assistantEnvelope,
  attachSpawnMock,
  exampleResponse,
  exampleUsage,
  FAKE_STDLIB_ROOT,
  fakeSender,
  primeChild,
  resultEnvelope,
  settle,
  type FakeChild,
} from './claudeAgentTestHarness'

const { spawnMock } = vi.hoisted(() => ({ spawnMock: vi.fn() }))

vi.mock('cross-spawn', () => ({ default: spawnMock }))
vi.mock('electron', () => ({ ipcMain: { handle: vi.fn(), on: vi.fn() } }))

const { ChildAgent } = await import('../../src/ai/claudeAgentChild')

interface ChildHarness {
  child: InstanceType<typeof ChildAgent>
  children: FakeChild[]
}

function buildChild(
  config: { stdlibRoot?: string | undefined; mcpConfigPath?: string | undefined } = {},
): ChildHarness {
  const { children } = attachSpawnMock(spawnMock)
  const child = new ChildAgent({
    stdlibRoot: 'stdlibRoot' in config ? config.stdlibRoot : FAKE_STDLIB_ROOT,
    mcpConfigPath: 'mcpConfigPath' in config ? config.mcpConfigPath : undefined,
  })
  return { child, children }
}

describe('ChildAgent', () => {
  beforeEach(() => {
    spawnMock.mockReset()
  })
  afterEach(() => {
    vi.useRealTimers()
    vi.clearAllMocks()
  })

  test('priming completes; first turn succeeds with usage', async () => {
    const { child, children } = buildChild()
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
    // With stdlib available, priming asks the agent to study the top-level + Base + Table
    // CLAUDE.mds and the Image source before it acknowledges. Other libraries' CLAUDE.mds are
    // loaded on demand. Path of the stdlib root is interpolated into the prompt.
    expect(children[0]!.stdinWrites[0]).toContain('CLAUDE.md')
    expect(children[0]!.stdinWrites[0]).toContain(FAKE_STDLIB_ROOT)
    expect(children[0]!.stdinWrites[0]).toContain('Base/0.0.0-dev/CLAUDE.md')
    expect(children[0]!.stdinWrites[0]).toContain('Table/0.0.0-dev/CLAUDE.md')
    expect(children[0]!.stdinWrites[0]).toContain('READY')

    const sender = fakeSender()
    const turnPromise = child.runTurn('user input', 60_000, sender, 'req-test')
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(2)
    expect(children[0]!.stdinWrites[1]).toContain('user input')

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const turn = await turnPromise
    expect(turn.state).toBe('completed')
    expect(turn.usage?.input_tokens).toBe(exampleUsage.input_tokens)
    expect(turn.usage?.output_tokens).toBe(exampleUsage.output_tokens)
    expect(turn.usage?.cache_read_input_tokens).toBe(exampleUsage.cache_read_input_tokens)
    expect(turn.usage?.cache_creation_input_tokens).toBe(exampleUsage.cache_creation_input_tokens)
    // No `assistant` envelope was pushed in this test, so `lastHopUsage` is null and
    // `hopCount` is 0 — the degenerate "no model output observed" case.
    expect(turn.lastHopUsage).toBeNull()
    expect(turn.hopCount).toBe(0)
    child.shutdown()
  })

  test('contextTokens comes from the LAST assistant envelope, not result.usage', async () => {
    const { child, children } = buildChild()
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

    const turnPromise = child.runTurn('go', 60_000, fakeSender(), 'req-test')
    await settle()
    children[0]!.pushStdoutLine(assistantEnvelope('thinking…', earlyHop))
    children[0]!.pushStdoutLine(assistantEnvelope(JSON.stringify(exampleResponse), finalHop))
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, resultUsage))
    const turn = await turnPromise

    // Cost-side roll-up comes from the result envelope's usage.
    expect(turn.usage).toEqual(resultUsage)
    // Last-hop usage tracks the FINAL assistant envelope (not the early one).
    expect(turn.lastHopUsage).toEqual(finalHop)
    expect(turn.hopCount).toBe(2)
    child.shutdown()
  })

  test('lastHopUsage is null when the final assistant envelope omits usage', async () => {
    // Captures the broken case: an early hop carries `usage`, but the final synthesis envelope
    // (post-tool) omits it. We must NOT keep the stale earlier value — `lastHopUsage` is
    // overwritten with `null` on every envelope so a later consumer can detect the fallback.
    const { child, children } = buildChild()
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

    const turnPromise = child.runTurn('go', 60_000, fakeSender(), 'req-test')
    await settle()
    children[0]!.pushStdoutLine(assistantEnvelope('thinking…', earlyHop))
    // Final envelope: no `usage` — simulates the broken case.
    children[0]!.pushStdoutLine(assistantEnvelope(JSON.stringify(exampleResponse)))
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, resultUsage))
    const turn = await turnPromise

    expect(turn.lastHopUsage).toBeNull()
    expect(turn.hopCount).toBe(2)
    expect(turn.usage).toEqual(resultUsage)
    child.shutdown()
  })

  test('crash mid-turn fails the turn and respawns', async () => {
    const { child, children } = buildChild()
    await primeChild(children[0]!)

    const turnPromise = child.runTurn('go', 60_000, fakeSender(), 'req-1')
    await settle()
    children[0]!.crash(1, 'authentication failure')
    const turn = await turnPromise
    expect(turn.state).toBe('crash')
    expect(turn.errorReason).toMatch(/exited with code=1/)

    // After the crash, spawn should have been called again automatically.
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(2)
    expect(children).toHaveLength(2)
    await primeChild(children[1]!)

    const turn2Promise = child.runTurn('go', 60_000, fakeSender(), 'req-2')
    await settle()
    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const turn2 = await turn2Promise
    expect(turn2.state).toBe('completed')
    child.shutdown()
  })

  test('crash-loop guard suspends respawn after three failures', async () => {
    const { child, children } = buildChild()
    // Crash three times in succession during priming (before READY is pushed).
    for (let i = 0; i < 3; i++) {
      await settle()
      children[children.length - 1]!.crash(1, 'boom')
      await settle()
    }
    // After the 3rd crash, respawn should have been suspended; spawn count caps at 3.
    expect(spawnMock).toHaveBeenCalledTimes(3)
    expect(child.respawnSuspended).toBe(true)

    // A manual respawn brings up a 4th child; if it crashes, the guard trips again.
    await child.respawn()
    expect(spawnMock).toHaveBeenCalledTimes(4)
    children[3]!.crash(1, 'still broken')
    await settle()
    expect(child.respawnSuspended).toBe(true)
    child.shutdown()
  })

  test('per-turn timeout returns crash without killing the child', async () => {
    const { child, children } = buildChild()
    // Prime with real timers (setImmediate-driven flush); switch to fake timers only for the
    // timeout assertion itself. ChildAgent's `runTurn` doesn't internally await `ready`, so
    // forcing the priming envelope through under fake timers races readline's microtask
    // scheduling against our second runTurn.
    await primeChild(children[0]!)
    vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] })

    const turnPromise = child.runTurn('go', 360_000, fakeSender(), 'req-1')
    for (let i = 0; i < 4; i++) await Promise.resolve()
    expect(children[0]!.stdinWrites.length).toBeGreaterThanOrEqual(2)

    await vi.advanceTimersByTimeAsync(361_000)
    const turn = await turnPromise
    expect(turn.state).toBe('crash')
    expect(turn.errorReason).toMatch(/timed out/)

    // Child should NOT have been killed by the timeout.
    expect(children[0]!.killCalls).toHaveLength(0)
    expect(spawnMock).toHaveBeenCalledTimes(1)

    child.shutdown()
  })

  test('omits --add-dir / --allowedTools when stdlib root cannot be resolved', () => {
    const { child } = buildChild({ stdlibRoot: undefined })
    expect(spawnMock).toHaveBeenCalledTimes(1)
    const [, args] = spawnMock.mock.calls[0]!
    expect(args).not.toContain('--add-dir')
    expect(args).not.toContain('--allowedTools')
    // The agent still spawns; it just runs without filesystem context.
    expect(args).toContain('--input-format')
    child.shutdown()
  })

  test('with mcpConfigPath, --mcp-config + --strict-mcp-config + tool allow-list are wired', () => {
    const { child } = buildChild({ mcpConfigPath: '/tmp/fake-mcp.json' })
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
    child.shutdown()
  })

  test('emits ai-progress: tool with raw input for built-in and MCP tool_use blocks', async () => {
    const { child, children } = buildChild()
    await primeChild(children[0]!)
    const sender = fakeSender()
    const sendMock = (sender as unknown as { send: ReturnType<typeof vi.fn> }).send
    const turnPromise = child.runTurn('go', 60_000, sender, 'req-tool')
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
    await turnPromise
    child.shutdown()
  })
})
