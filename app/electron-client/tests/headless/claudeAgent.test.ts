/**
 * @file Unit tests for the session-level concerns of {@link ClaudeAgentSession}: queue
 * serialization, IPC sender handling, cancellation routing, shutdown cascade, and the
 * `queued`/`started`/`text` progress events that the session emits or relays. Per-child
 * concerns (priming, stdout parsing, spawn flags, crash + respawn, timeout, tool events) are
 * exercised in `claudeAgentChild.test.ts`.
 */
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import {
  attachSpawnMock,
  exampleRequest,
  exampleResponse,
  exampleUsage,
  FAKE_STDLIB_ROOT,
  fakeSender,
  makeRequest,
  primeChild,
  resultEnvelope,
  settle,
  type FakeChild,
} from './claudeAgentTestHarness'

const { spawnMock } = vi.hoisted(() => ({ spawnMock: vi.fn() }))

vi.mock('cross-spawn', () => ({ default: spawnMock }))
vi.mock('electron', () => ({ ipcMain: { handle: vi.fn(), on: vi.fn() } }))

const { ClaudeAgentSession, initClaudeAgentIpc, shutdownClaudeAgent } = await import(
  '../../src/ai/claudeAgent'
)
const { ipcMain } = await import('electron')
const { Channel } = await import('../../src/ipc.js')

interface SessionHarness {
  session: InstanceType<typeof ClaudeAgentSession>
  children: FakeChild[]
}

function buildSession(
  config: {
    stdlibRoot?: string | undefined
    mcpConfigPath?: string | undefined
    softThreshold?: number
    hardThreshold?: number
  } = {},
): SessionHarness {
  const { children } = attachSpawnMock(spawnMock)
  const session = new ClaudeAgentSession({
    stdlibRoot: 'stdlibRoot' in config ? config.stdlibRoot : FAKE_STDLIB_ROOT,
    mcpConfigPath: 'mcpConfigPath' in config ? config.mcpConfigPath : undefined,
    ...('softThreshold' in config ? { softThreshold: config.softThreshold } : {}),
    ...('hardThreshold' in config ? { hardThreshold: config.hardThreshold } : {}),
  })
  return { session, children }
}

/**
 * Mint a `result.usage` payload whose `input_tokens` equals the desired contextTokens (with
 * cache fields zero). When no assistant envelope carries `usage`, the session's
 * `snapshotUsage` falls back to the result-envelope sum, so this number drives the
 * threshold-evaluation directly.
 */
function highContextUsage(contextTokens: number): typeof exampleUsage {
  /* eslint-disable camelcase */
  return {
    input_tokens: contextTokens,
    output_tokens: 100,
    cache_read_input_tokens: 0,
    cache_creation_input_tokens: 0,
  }
  /* eslint-enable camelcase */
}

describe('ClaudeAgentSession', () => {
  beforeEach(() => {
    spawnMock.mockReset()
  })
  afterEach(() => {
    vi.useRealTimers()
    vi.clearAllMocks()
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

  test('cancelTurn on the in-flight request resolves with cancellation Err and writes a control_request', async () => {
    // Modern Claude Code honors an in-band `control_request`/`interrupt` envelope, so the
    // common cancel path no longer kills the child — the warm conversation context is kept.
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
    // No process signals — cancel goes through stdin.
    expect(children[0]!.killCalls).toHaveLength(0)
    // Last stdin write is the SDK-shaped interrupt envelope.
    const lastWrite = children[0]!.stdinWrites[children[0]!.stdinWrites.length - 1]!
    const envelope = JSON.parse(lastWrite.trim())
    expect(envelope.type).toBe('control_request')
    expect(envelope.request).toEqual({ subtype: 'interrupt' })
    session.shutdown()
  })

  test('a queued request submitted right after a cancel runs on the SAME child (no respawn)', async () => {
    // With the in-band interrupt path, cancel does NOT exit the child. The next queued request
    // reuses the same primed child; warm conversation context is preserved.
    const { session, children } = buildSession()
    await primeChild(children[0]!)

    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(2) // priming + r1
    session.cancelTurn('r1')
    const reply1 = await r1
    expect(reply1.result.ok).toBe(false)
    if (!reply1.result.ok) expect(reply1.result.error.payload).toMatch(/cancelled by user/)
    expect(children[0]!.killCalls).toHaveLength(0)

    // Push the wire sequence the CLI would emit after a successful interrupt: control_response
    // success + the synthetic user echo + the aborted_streaming result envelope.
    const interruptEnvelope = JSON.parse(
      children[0]!.stdinWrites[children[0]!.stdinWrites.length - 1]!.trim(),
    )
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'control_response',
        // eslint-disable-next-line camelcase
        response: { subtype: 'success', request_id: interruptEnvelope.request_id },
      }),
    )
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'user',
        message: {
          role: 'user',
          content: [{ type: 'text', text: '[Request interrupted by user]' }],
        },
      }),
    )
    children[0]!.pushStdoutLine(
      JSON.stringify({
        type: 'result',
        subtype: 'error_during_execution',
        /* eslint-disable camelcase */
        is_error: true,
        terminal_reason: 'aborted_streaming',
        /* eslint-enable camelcase */
        result: '',
      }),
    )

    // Request 2 lands on the same primed child — no respawn, no priming overhead.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(1)
    expect(children).toHaveLength(1)
    expect(children[0]!.stdinWrites).toHaveLength(4) // priming + r1 + control_request + r2

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
    const reply2 = await r2
    expect(reply2.result.ok).toBe(true)
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

  // ----- Context-rotation tests -----

  test('below soft threshold → no warming spawned', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    const replyPromise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    await replyPromise
    await settle()

    expect(spawnMock).toHaveBeenCalledTimes(1)
    expect(children).toHaveLength(1)
    session.shutdown()
  })

  test('above soft, below hard → warming spawned; next turn still on old', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r1
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(2)
    // Warming has been spawned but its `READY` hasn't been pushed yet.
    expect(children[1]!.stdinWrites).toHaveLength(1)

    // Second request — warming is still priming, so the request must still go to primary.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(3) // priming + r1 + r2
    expect(children[1]!.stdinWrites).toHaveLength(1) // warming priming only

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    await r2
    session.shutdown()
  })

  test('soft → warming primes → next turn runs on new child, old shut down', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    // r1 ran on the still-original primary — `freshAgent` must be false because no rotation
    // has fired yet (the warming child is being spawned in r1's `finally`).
    const r1Reply = await r1
    expect(r1Reply.usage?.freshAgent).toBe(false)
    await settle()
    expect(children).toHaveLength(2)

    // Prime warming. Promotion happens at the next queue task's start.
    await primeChild(children[1]!)

    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[1]!.stdinWrites).toHaveLength(2) // priming + r2
    // Old primary should have been SIGTERM'd by the promotion.
    expect(children[0]!.killCalls).toContain('SIGTERM')

    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    const reply = await r2
    expect(reply.result.ok).toBe(true)
    // r2 is the first turn on the freshly-promoted child — the rotation flag must fire here.
    expect(reply.usage?.freshAgent).toBe(true)

    // A subsequent turn on the same (now-stable) primary must NOT see the flag again.
    const r3 = session.runRequest(makeRequest({ requestId: 'r3' }), fakeSender())
    await settle()
    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    const r3Reply = await r3
    expect(r3Reply.usage?.freshAgent).toBe(false)
    session.shutdown()
  })

  test('above hard → next turn waits for warming.ready before writing stdin', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(250_000)))
    await r1
    await settle()
    expect(children).toHaveLength(2)

    // r2 should park on warming.ready — neither child receives it yet.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(2) // priming + r1 only
    expect(children[1]!.stdinWrites).toHaveLength(1) // warming priming only

    await primeChild(children[1]!)
    await settle()
    // r2 now runs on warming (now-promoted primary).
    expect(children[1]!.stdinWrites).toHaveLength(2)

    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    await r2
    session.shutdown()
  })

  test('soft transitions to hard when a later turn pushes contextTokens past hard', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    // Turn 1 → soft swap.
    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r1
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(2)

    // Turn 2 (warming still priming) — runs on primary, returns hard usage.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(3)
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(250_000)))
    await r2
    await settle()
    // No new warming was spawned — we just upgraded the existing one to hard.
    expect(spawnMock).toHaveBeenCalledTimes(2)

    // Turn 3 should now block on warming.ready (hard mode).
    const r3 = session.runRequest(makeRequest({ requestId: 'r3' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(3) // r3 hasn't run on primary
    expect(children[1]!.stdinWrites).toHaveLength(1) // warming priming, not r3

    await primeChild(children[1]!)
    await settle()
    expect(children[1]!.stdinWrites).toHaveLength(2) // priming + r3
    children[1]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    await r3
    session.shutdown()
  })

  test('freshAgent flag survives a crashed turn and lands on the next successful reply', async () => {
    // Regression guard for the `freshAgentPending` semantic: a crashed turn carries no `usage`,
    // so its reply doesn't surface the flag to the renderer. The flag must remain armed and
    // fire on the first subsequent turn whose reply *does* carry usage — otherwise a
    // mid-rotation crash would silently swallow the rotation signal that e2e tests rely on.
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    // Turn 1: cross soft to spawn warming.
    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r1
    await settle()
    expect(children).toHaveLength(2)

    // Prime warming so the next queue task promotes it.
    await primeChild(children[1]!)

    // Turn 2: runs on freshly-promoted primary, then crashes mid-turn (reply.usage is null).
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    children[1]!.crash(1, 'boom')
    const r2Reply = await r2
    expect(r2Reply.result.ok).toBe(false)
    expect(r2Reply.usage).toBeNull()
    await settle()

    // Auto-respawn produced a fresh child to replace the crashed primary. Prime it.
    expect(children.length).toBeGreaterThanOrEqual(3)
    await primeChild(children[2]!)

    // Turn 3: first reply after rotation that actually carries usage; the flag surfaces here.
    const r3 = session.runRequest(makeRequest({ requestId: 'r3' }), fakeSender())
    await settle()
    children[2]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    const r3Reply = await r3
    expect(r3Reply.result.ok).toBe(true)
    expect(r3Reply.usage?.freshAgent).toBe(true)
    session.shutdown()
  })

  test('warming priming fails (soft) → discarded on next task; swap re-arms on next over-threshold turn', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    // Turn 1 → soft swap.
    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r1
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(2)

    // Crash warming three times to trip the crash-loop guard.
    for (let i = 0; i < 3; i++) {
      children[children.length - 1]!.crash(1, 'boom')
      await settle()
    }
    // After 3 crashes, warming is suspended (children may have grown via auto-respawn).
    expect(spawnMock).toHaveBeenCalledTimes(4)

    // Next request: promotePrimedWarming detects suspended warming and discards.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(3) // r2 ran on primary

    // Push an over-threshold result to re-arm a fresh warming.
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r2
    await settle()
    expect(spawnMock).toHaveBeenCalledTimes(5) // a new warming was spawned
    session.shutdown()
  })

  test('warming priming fails (hard) → awaiting prompt falls back to primary', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    // Turn 1 → hard swap.
    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(250_000)))
    await r1
    await settle()
    expect(children.length).toBeGreaterThanOrEqual(2)
    const warmingChild = children[1]!

    // r2 should park on warming.ready.
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(2) // priming + r1 only
    expect(warmingChild.stdinWrites).toHaveLength(1) // warming priming only

    // A single warming crash → ChildAgent rejects readyDeferred → hard-mode catch fires →
    // warming is shut down and r2 falls back to primary.
    warmingChild.crash(1, 'boom')
    await settle()

    expect(children[0]!.stdinWrites).toHaveLength(3) // priming + r1 + r2 fell back to primary
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    const reply = await r2
    expect(reply.result.ok).toBe(true)
    session.shutdown()
  })

  test('in-flight on old primary is not cancelled when threshold crosses', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    const r1Promise = session.runRequest(exampleRequest, fakeSender())
    await settle()
    expect(children[0]!.killCalls).toHaveLength(0)

    // Push the over-threshold result. Threshold evaluation happens in the queue task's
    // `finally` — by which time r1 has already settled normally.
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(250_000)))
    const reply = await r1Promise
    expect(reply.result.ok).toBe(true)
    // The rotation must NEVER signal old primary just to swap it out.
    expect(children[0]!.killCalls).toHaveLength(0)

    await settle()
    // Warming was spawned for the next turn.
    expect(children).toHaveLength(2)
    session.shutdown()
  })

  test('env var ENSO_AI_SOFT_CONTEXT_THRESHOLD is honored', async () => {
    vi.stubEnv('ENSO_AI_SOFT_CONTEXT_THRESHOLD', '1')
    vi.stubEnv('ENSO_AI_HARD_CONTEXT_THRESHOLD', '2')
    try {
      const { session, children } = buildSession()
      await primeChild(children[0]!)

      const replyPromise = session.runRequest(exampleRequest, fakeSender())
      await settle()
      // Default `exampleUsage` carries 1234 + 5000 + 2000 = 8234 contextTokens via the result
      // envelope's roll-up — easily above the env-var-overridden thresholds.
      children[0]!.pushStdoutLine(resultEnvelope(exampleResponse))
      await replyPromise
      await settle()
      expect(spawnMock).toHaveBeenCalledTimes(2) // primary + warming
      session.shutdown()
    } finally {
      vi.unstubAllEnvs()
    }
  })

  test('activeRequest reflects the in-flight request mid-swap', async () => {
    const { session, children } = buildSession({ softThreshold: 100_000, hardThreshold: 200_000 })
    await primeChild(children[0]!)

    // Trigger a soft swap.
    const r1 = session.runRequest(makeRequest({ requestId: 'r1' }), fakeSender())
    await settle()
    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(150_000)))
    await r1
    await settle()
    expect(children).toHaveLength(2)

    // Issue r2 while warming is still priming. Soft mode → r2 runs on primary.
    const sender = fakeSender()
    const r2 = session.runRequest(makeRequest({ requestId: 'r2' }), sender)
    await settle()
    expect(session.activeRequest).toEqual({ requestId: 'r2', sender })

    children[0]!.pushStdoutLine(resultEnvelope(exampleResponse, highContextUsage(50_000)))
    await r2
    expect(session.activeRequest).toBeNull()
    session.shutdown()
  })

  // ----- end of context-rotation tests -----

  test('cancelTurn during the primary-ready await short-circuits before stdin is written', async () => {
    // Regression guard for the cancel-during-await race: a cancel arriving while the queue task
    // is parked on `await primary.ready` is filed into the `cancelled` set (no pending slot
    // matches the user's id yet). The entry-check at the top of the queue task already ran, so
    // the re-check immediately before `runTurn` is the only window that observes this cancel.
    const { session, children } = buildSession()
    // Issue the request before priming completes so the queue task parks on `primary.ready`.
    const request = makeRequest({ requestId: 'req-during-await' })
    const replyPromise = session.runRequest(request, fakeSender())
    await settle()
    expect(children[0]!.stdinWrites).toHaveLength(1) // priming only — user prompt not yet written

    session.cancelTurn(request.requestId)
    await settle()
    // Even after another settle, no user prompt has hit stdin — the queue task is still parked.
    expect(children[0]!.stdinWrites).toHaveLength(1)

    // Resume the queue task by completing priming. The re-check must consume the deferred cancel.
    await primeChild(children[0]!)
    const reply = await replyPromise
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok) expect(reply.result.error.payload).toMatch(/Cancelled by user/)
    // The user prompt must never have been written.
    expect(children[0]!.stdinWrites).toHaveLength(1)
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

  test('isAvailable resolves true once cross-spawn returns an alive child', async () => {
    const { session, children } = buildSession()
    // The fake child returned by `attachSpawnMock` is alive from construction time, so its
    // underlying `firstSpawn` promise resolves on the next microtask.
    expect(children).toHaveLength(1)
    await expect(session.isAvailable).resolves.toBe(true)
    session.shutdown()
  })

  test('isAvailable resolves false when cross-spawn throws synchronously (ENOENT)', async () => {
    // Wire the mock to throw ENOENT directly (no `attachSpawnMock` here — it would override
    // the mock back to a happy-path child). The `WatchedChildProcess` wrapper catches the
    // synchronous throw inside `spawnNext` and rejects `firstSpawn`, which `firstSpawnSettled`
    // converts to `false`.
    spawnMock.mockImplementation(() => {
      const err = new Error("spawn 'claude' ENOENT") as NodeJS.ErrnoException
      err.code = 'ENOENT'
      throw err
    })
    const session = new ClaudeAgentSession({
      stdlibRoot: FAKE_STDLIB_ROOT,
      mcpConfigPath: undefined,
    })
    await expect(session.isAvailable).resolves.toBe(false)
    session.shutdown()
  })
})

describe('initClaudeAgentIpc', () => {
  beforeEach(() => {
    spawnMock.mockReset()
    vi.mocked(ipcMain.handle).mockReset()
    vi.mocked(ipcMain.on).mockReset()
  })
  afterEach(() => {
    shutdownClaudeAgent()
    vi.unstubAllEnvs()
  })

  test('without ENSO_AI_DISABLED: spawns the session and registers live IPC handlers', async () => {
    attachSpawnMock(spawnMock)
    initClaudeAgentIpc({ stdlibRoot: FAKE_STDLIB_ROOT, mcpConfigPath: undefined })

    // Session construction triggers cross-spawn for the primary child.
    expect(spawnMock).toHaveBeenCalled()

    const handleCalls = vi.mocked(ipcMain.handle).mock.calls
    const aiIsAvailableHandler = handleCalls.find((call) => call[0] === Channel.aiIsAvailable)?.[1]
    expect(aiIsAvailableHandler).toBeDefined()
    // FakeChild is alive from construction, so `firstSpawnSettled` resolves true.
    await expect((aiIsAvailableHandler as () => Promise<boolean>)()).resolves.toBe(true)

    expect(handleCalls.some((call) => call[0] === Channel.generateAiComponent)).toBe(true)
    const onCalls = vi.mocked(ipcMain.on).mock.calls
    expect(onCalls.some((call) => call[0] === Channel.cancelAiComponent)).toBe(true)
  })

  test('with ENSO_AI_DISABLED=1: skips child spawn and registers disabled-mode IPC handlers', async () => {
    vi.stubEnv('ENSO_AI_DISABLED', '1')
    initClaudeAgentIpc({ stdlibRoot: FAKE_STDLIB_ROOT, mcpConfigPath: undefined })

    expect(spawnMock).not.toHaveBeenCalled()

    const handleCalls = vi.mocked(ipcMain.handle).mock.calls
    const aiIsAvailableHandler = handleCalls.find((call) => call[0] === Channel.aiIsAvailable)?.[1]
    expect(aiIsAvailableHandler).toBeDefined()
    await expect((aiIsAvailableHandler as () => Promise<boolean>)()).resolves.toBe(false)

    const generateHandler = handleCalls.find((call) => call[0] === Channel.generateAiComponent)?.[1]
    expect(generateHandler).toBeDefined()
    const reply = await (
      generateHandler as () => Promise<{
        result: { ok: boolean; error?: { payload: string } }
      }>
    )()
    expect(reply.result.ok).toBe(false)
    if (!reply.result.ok) expect(reply.result.error!.payload).toMatch(/ENSO_AI_DISABLED/)

    const onCalls = vi.mocked(ipcMain.on).mock.calls
    expect(onCalls.some((call) => call[0] === Channel.cancelAiComponent)).toBe(true)
  })
})
