/** @file Unit tests for the renderer-side AI prompt queue. Focused on enqueue, cancel, and refresh. */

import { ongoingAiPromptsStoreFactory, type OngoingAiPromptsDeps } from '@/stores/ongoingAiPrompts'
import { Vec2 } from '@/util/data/vec2'
import { withSetup } from '@/util/testing'
import type { AiComponentResponse, AiProgressEvent } from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { describe, expect, test, vi, type Mock } from 'vitest'
import { computed, nextTick } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

interface Deferred<T> {
  promise: Promise<T>
  resolve: (value: T) => void
  reject: (error: unknown) => void
}

function deferred<T>(): Deferred<T> {
  let resolve!: (value: T) => void
  let reject!: (error: unknown) => void
  const promise = new Promise<T>((res, rej) => {
    resolve = res
    reject = rej
  })
  return { promise, resolve, reject }
}

interface Harness {
  store: ReturnType<typeof ongoingAiPromptsStoreFactory>
  dispatch: Mock
  dispatchCalls: Deferred<Result<AiComponentResponse>>[]
  toastShow: Mock
  cancelIpc: Mock
  emitProgress: (event: AiProgressEvent) => void
}

function setupHarness(): Harness {
  const dispatchCalls: Deferred<Result<AiComponentResponse>>[] = []
  const dispatch = vi.fn(() => {
    const d = deferred<Result<AiComponentResponse>>()
    dispatchCalls.push(d)
    return d.promise
  })
  const toastShow = vi.fn()
  const cancelIpc = vi.fn()
  const progressHandlers: Array<(event: AiProgressEvent) => void> = []

  const electronApi = {
    ai: {
      onProgress: vi.fn((handler: (event: AiProgressEvent) => void) => {
        progressHandlers.push(handler)
        return () => {
          const i = progressHandlers.indexOf(handler)
          if (i !== -1) progressHandlers.splice(i, 1)
        }
      }),
      cancel: cancelIpc,
    },
  } as unknown as typeof window.api

  const methodId = 'method-1' as ExternalId
  const bodyId = 'body-1' as ExternalId
  const graphStore = {
    db: { nodeIdToNode: new Map() },
    currentMethod: {
      ast: Ok({ externalId: methodId, body: { externalId: bodyId } }),
      pointer: Ok({ name: 'method_one' }),
    },
    registerExtraOccupiedAreas: vi.fn(() => () => {}),
    generateLocallyUniqueIdent: (s: string) => s,
  } as unknown as OngoingAiPromptsDeps['graphStore']

  const module = computed(
    () =>
      ({
        root: undefined,
        edit: vi.fn(),
      }) as unknown as OngoingAiPromptsDeps['module']['value'],
  )

  const store = withSetup(() =>
    ongoingAiPromptsStoreFactory({
      graphStore,
      module,
      ai: { dispatch },
      toastError: { show: toastShow },
      electronApi,
    }),
  )

  return {
    store,
    dispatch,
    dispatchCalls,
    toastShow,
    cancelIpc,
    emitProgress: (event: AiProgressEvent) => progressHandlers.forEach((h) => h(event)),
  }
}

function enqueueOne(harness: Harness, prompt: string): string {
  return harness.store.enqueue({
    prompt,
    sourceIdentifier: undefined,
    methodId: 'method-1' as ExternalId,
    methodBodyId: 'body-1' as ExternalId,
    methodName: 'method_one',
    position: new Vec2(10, 20),
  })
}

describe('enqueue + dispatcher', () => {
  test('first enqueue dispatches synchronously', () => {
    const h = setupHarness()
    enqueueOne(h, 'hello')
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)
    const entry = h.store.entriesForCurrentMethod[0]!
    expect(entry.prompt).toBe('hello')
    expect(entry.dispatched).toBe(true)
    expect(h.dispatch).toHaveBeenCalledTimes(1)
  })

  test('second enqueue waits behind the first', () => {
    const h = setupHarness()
    enqueueOne(h, 'first')
    enqueueOne(h, 'second')
    expect(h.store.entriesForCurrentMethod).toHaveLength(2)
    expect(h.store.entriesForCurrentMethod[0]!.dispatched).toBe(true)
    expect(h.store.entriesForCurrentMethod[1]!.dispatched).toBe(false)
    expect(h.dispatch).toHaveBeenCalledTimes(1)
  })

  test('started progress event flips the entry from queued to running', () => {
    const h = setupHarness()
    enqueueOne(h, 'hello')
    const entry = h.store.entriesForCurrentMethod[0]!
    expect(entry.status).toBe('queued')
    expect(entry.statusText).toBe('Waiting…')

    h.emitProgress({ kind: 'started', requestId: entry.requestId })

    expect(entry.status).toBe('running')
    expect(entry.statusText).toBe('Thinking…')
  })

  test('after a successful completion, the next enqueue dispatches the new entry', async () => {
    const h = setupHarness()
    enqueueOne(h, 'first')
    const firstEntry = h.store.entriesForCurrentMethod[0]!
    h.dispatchCalls[0]!.resolve(
      Ok({
        functionName: 'helper',
        argumentNames: [],
        body: 'x = 1\nx',
        callArguments: [],
      }) as Result<AiComponentResponse>,
    )
    // Let runEntry resume, hit commit(), and remove the entry from the map.
    await nextTick()
    await nextTick()
    // commit will toast-fail because our stub module has no root; the dispatcher must still
    // release `dispatching` and proceed to the next entry.
    enqueueOne(h, 'second')
    const secondEntry = h.store.entriesForCurrentMethod.find((e) => e.prompt === 'second')!
    expect(secondEntry.dispatched).toBe(true)
    expect(h.dispatch).toHaveBeenCalledTimes(2)
    expect(h.dispatch.mock.calls[1]![0]).toBe('second')
    expect(h.dispatch.mock.calls[1]![2]).toBe(secondEntry.requestId)
    void firstEntry
  })
})

describe('cancel', () => {
  test('cancel on a not-yet-dispatched entry deletes locally without IPC', () => {
    const h = setupHarness()
    enqueueOne(h, 'first')
    const secondId = enqueueOne(h, 'second')
    h.store.cancel(secondId)
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)
    expect(h.store.entriesForCurrentMethod[0]!.prompt).toBe('first')
    expect(h.cancelIpc).not.toHaveBeenCalled()
  })

  test('cancel on a dispatched entry sends IPC and keeps the entry until the reply', () => {
    const h = setupHarness()
    const id = enqueueOne(h, 'first')
    const entry = h.store.entriesForCurrentMethod[0]!
    h.store.cancel(id)
    expect(h.cancelIpc).toHaveBeenCalledTimes(1)
    expect(h.cancelIpc).toHaveBeenCalledWith(entry.requestId)
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)
  })

  test('cancel on a failed entry deletes locally without IPC', async () => {
    const h = setupHarness()
    enqueueOne(h, 'first')
    h.dispatchCalls[0]!.resolve(Err('boom'))
    await nextTick()
    await nextTick()
    expect(h.store.entriesForCurrentMethod[0]!.status).toBe('failed')
    const id = h.store.entriesForCurrentMethod[0]!.id
    h.store.cancel(id)
    expect(h.store.entriesForCurrentMethod).toHaveLength(0)
    expect(h.cancelIpc).not.toHaveBeenCalled()
  })
})

describe('refresh', () => {
  test('refresh on a failed entry replaces it with a fresh queued entry', async () => {
    const h = setupHarness()
    enqueueOne(h, 'first')
    const originalEntry = h.store.entriesForCurrentMethod[0]!
    const originalId = originalEntry.id
    const originalRequestId = originalEntry.requestId
    const originalPosition = originalEntry.position

    h.dispatchCalls[0]!.resolve(Err('boom'))
    await nextTick()
    await nextTick()
    expect(h.store.entriesForCurrentMethod[0]!.status).toBe('failed')

    h.store.refresh(originalId)
    expect(h.cancelIpc).not.toHaveBeenCalled()
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)

    const newEntry = h.store.entriesForCurrentMethod[0]!
    expect(newEntry.id).not.toBe(originalId)
    expect(newEntry.requestId).not.toBe(originalRequestId)
    expect(newEntry.prompt).toBe('first')
    expect(newEntry.position).toEqual(originalPosition)
    expect(newEntry.status).toBe('queued')
    expect(newEntry.dispatched).toBe(true)
    expect(h.dispatch).toHaveBeenCalledTimes(2)
  })

  test('refresh on a running entry sends cancel IPC and enqueues a fresh entry', () => {
    const h = setupHarness()
    const id = enqueueOne(h, 'first')
    const originalEntry = h.store.entriesForCurrentMethod[0]!
    const originalRequestId = originalEntry.requestId

    h.store.refresh(id)
    expect(h.cancelIpc).toHaveBeenCalledTimes(1)
    expect(h.cancelIpc).toHaveBeenCalledWith(originalRequestId)
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)

    const newEntry = h.store.entriesForCurrentMethod[0]!
    expect(newEntry.id).not.toBe(id)
    expect(newEntry.requestId).not.toBe(originalRequestId)
    expect(newEntry.prompt).toBe('first')
    // Dispatcher is still awaiting the original dispatch promise, so the new entry stays queued.
    expect(newEntry.status).toBe('queued')
    expect(newEntry.dispatched).toBe(false)
    expect(h.dispatch).toHaveBeenCalledTimes(1)
  })

  test('after refresh-on-running, the cancellation reply unblocks the new dispatch', async () => {
    const h = setupHarness()
    const id = enqueueOne(h, 'first')
    h.store.refresh(id)
    const newEntry = h.store.entriesForCurrentMethod[0]!

    // Resolve the original dispatch with the cancellation reply the main process would send.
    h.dispatchCalls[0]!.resolve(Err('Request cancelled by user.'))
    await nextTick()
    await nextTick()

    expect(h.dispatch).toHaveBeenCalledTimes(2)
    const secondCallRequestId = h.dispatch.mock.calls[1]![2]
    expect(secondCallRequestId).toBe(newEntry.requestId)
    expect(h.store.entriesForCurrentMethod).toHaveLength(1)
    expect(h.store.entriesForCurrentMethod[0]!.dispatched).toBe(true)
  })

  test('refresh on a missing id is a no-op', () => {
    const h = setupHarness()
    h.store.refresh('does-not-exist')
    expect(h.store.entriesForCurrentMethod).toHaveLength(0)
    expect(h.dispatch).not.toHaveBeenCalled()
    expect(h.cancelIpc).not.toHaveBeenCalled()
  })
})
