/** @file Tests for `ChildProcessHandle` and `WatchedChildProcess`. */

import type { ChildProcess } from 'node:child_process'
import { EventEmitter } from 'node:events'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import {
  ChildProcessHandle,
  WatchedChildProcess,
  type WatchedChildProcessOptions,
} from '../childProcess'

/**
 * Minimal stand-in for `ChildProcess`. Records `kill` calls and exposes `simulateExit` so a
 * test can drive the lifecycle without spawning a real process.
 */
class FakeChildProcess extends EventEmitter {
  killCalls: NodeJS.Signals[] = []

  kill(signal?: NodeJS.Signals | number): boolean {
    const sig = (typeof signal === 'string' ? signal : 'SIGTERM') as NodeJS.Signals
    this.killCalls.push(sig)
    return true
  }

  simulateExit(code: number | null, signal: NodeJS.Signals | null = null): void {
    this.emit('exit', code, signal)
  }

  simulateError(err: NodeJS.ErrnoException): void {
    this.emit('error', err)
  }
}

function enoent(message = 'spawn claude ENOENT'): NodeJS.ErrnoException {
  return Object.assign(new Error(message), { code: 'ENOENT' })
}

function asChild(fake: FakeChildProcess): ChildProcess {
  return fake as unknown as ChildProcess
}

beforeEach(() => {
  vi.useFakeTimers()
})
afterEach(() => {
  vi.useRealTimers()
})

/**
 * Drain queued microtasks. Needed between simulating an exit and asserting on the resulting
 * respawn, since the watcher's exit handler chains through several `await` steps.
 */
async function flush(): Promise<void> {
  await vi.advanceTimersByTimeAsync(0)
}

describe('ChildProcessHandle', () => {
  test('starts alive with no exit reason', () => {
    const handle = new ChildProcessHandle(asChild(new FakeChildProcess()))
    expect(handle.alive).toBe(true)
    expect(handle.exitReason).toBeNull()
  })

  test('records exit code and signal', () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    fake.simulateExit(0, null)
    expect(handle.alive).toBe(false)
    expect(handle.exitReason).toBe('exited with code=0 signal=null')
  })

  test('formats signal-only exits', () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    fake.simulateExit(null, 'SIGTERM')
    expect(handle.exitReason).toBe('exited with code=null signal=SIGTERM')
  })

  test('waitForExit resolves with code and signal', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const exit = handle.waitForExit()
    fake.simulateExit(42, null)
    expect(await exit).toEqual({ code: 42, signal: null })
  })

  test('waitForExit resolves immediately if the process has already exited', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    fake.simulateExit(0, null)
    expect(await handle.waitForExit()).toEqual({ code: 0, signal: null })
  })

  test('terminate sends SIGTERM and resolves on exit', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const done = handle.terminate()
    expect(fake.killCalls).toEqual(['SIGTERM'])
    fake.simulateExit(0, 'SIGTERM')
    await done
  })

  test('terminate is a no-op when the process has already exited', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    fake.simulateExit(0, null)
    await handle.terminate()
    expect(fake.killCalls).toEqual([])
  })

  test('terminate accepts a custom signal', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const done = handle.terminate({ signal: 'SIGINT' })
    expect(fake.killCalls).toEqual(['SIGINT'])
    fake.simulateExit(null, 'SIGINT')
    await done
  })

  test('terminate escalates to SIGKILL after the timeout', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const done = handle.terminate({ timeoutMs: 1_000 })
    expect(fake.killCalls).toEqual(['SIGTERM'])
    await vi.advanceTimersByTimeAsync(1_000)
    expect(fake.killCalls).toEqual(['SIGTERM', 'SIGKILL'])
    fake.simulateExit(null, 'SIGKILL')
    await done
  })

  test('terminate clears the SIGKILL timer once the process has exited', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const done = handle.terminate({ timeoutMs: 1_000 })
    fake.simulateExit(0, 'SIGTERM')
    await done
    await vi.advanceTimersByTimeAsync(5_000)
    expect(fake.killCalls).toEqual(['SIGTERM'])
  })

  test("'error' event marks handle dead and exposes exitError with the original code", () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const err = enoent()
    fake.simulateError(err)
    expect(handle.alive).toBe(false)
    expect(handle.exitError).toBe(err)
    expect(handle.exitReason).toContain('ENOENT')
  })

  test("'error' before 'exit' wins; subsequent 'exit' is a no-op", () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const err = enoent()
    fake.simulateError(err)
    fake.simulateExit(1, null)
    expect(handle.exitError).toBe(err)
    expect(handle.exitReason).toContain('ENOENT')
  })

  test("'exit' before 'error' wins; subsequent 'error' is a no-op", () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    fake.simulateExit(1, null)
    fake.simulateError(enoent())
    expect(handle.exitError).toBeNull()
    expect(handle.exitReason).toBe('exited with code=1 signal=null')
  })

  test('waitForExit resolves when only an error fires', async () => {
    const fake = new FakeChildProcess()
    const handle = new ChildProcessHandle(asChild(fake))
    const exit = handle.waitForExit()
    fake.simulateError(enoent())
    expect(await exit).toEqual({ code: null, signal: null })
  })
})

describe('WatchedChildProcess', () => {
  type SpawnerControl = {
    spawner: () => ChildProcess
    fakes: FakeChildProcess[]
    /** Make the next call to `spawner` throw `err` instead of returning a fake. */
    failNext: (err: unknown) => void
  }

  function makeSpawner(): SpawnerControl {
    const fakes: FakeChildProcess[] = []
    let nextError: { err: unknown } | null = null
    const spawner = (): ChildProcess => {
      if (nextError !== null) {
        const { err } = nextError
        nextError = null
        throw err
      }
      const fake = new FakeChildProcess()
      fakes.push(fake)
      return asChild(fake)
    }
    return {
      spawner,
      fakes,
      failNext(err) {
        nextError = { err }
      },
    }
  }

  function defaultOptions(): WatchedChildProcessOptions {
    return { onChildStarted: () => undefined }
  }

  /**
   * `close()` waits for the current child to exit. The fake never exits on its own, so this
   * helper triggers an exit on every fake (no-op for ones that already exited, since the
   * handle's listener is `once`) before awaiting the close.
   */
  async function teardown(
    watcher: WatchedChildProcess,
    fakes: readonly FakeChildProcess[],
  ): Promise<void> {
    const closeP = watcher.close()
    for (const fake of fakes) fake.simulateExit(0, 'SIGTERM')
    await closeP
  }

  test('starts the first child and exposes it via firstSpawn and current', async () => {
    const ctrl = makeSpawner()
    const onChildStarted = vi.fn()
    const watcher = new WatchedChildProcess(ctrl.spawner, { onChildStarted })
    const handle = await watcher.firstSpawn
    expect(ctrl.fakes).toHaveLength(1)
    expect(onChildStarted).toHaveBeenCalledTimes(1)
    expect(onChildStarted).toHaveBeenCalledWith(handle)
    expect(watcher.current).toBe(handle)
    expect(handle.alive).toBe(true)
    await teardown(watcher, ctrl.fakes)
  })

  test('rejects firstSpawn when the initial spawner throws', async () => {
    const ctrl = makeSpawner()
    const err = new Error('boom')
    ctrl.failNext(err)
    const onUnexpectedExit = vi.fn()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      ...defaultOptions(),
      onUnexpectedExit,
    })
    await expect(watcher.firstSpawn).rejects.toBe(err)
    expect(onUnexpectedExit).not.toHaveBeenCalled()
    expect(watcher.current).toBeNull()
    expect(watcher.respawnSuspended).toBeTruthy()
  })

  test('async spawner is awaited before onChildStarted fires', async () => {
    let resolveSpawner: (value: ChildProcess) => void = () => {}
    const fake = new FakeChildProcess()
    const onChildStarted = vi.fn()
    const watcher = new WatchedChildProcess(
      () =>
        new Promise<ChildProcess>((resolve) => {
          resolveSpawner = resolve
        }),
      { onChildStarted },
    )
    await flush()
    expect(onChildStarted).not.toHaveBeenCalled()
    resolveSpawner(asChild(fake))
    await watcher.firstSpawn
    expect(onChildStarted).toHaveBeenCalledTimes(1)
    await teardown(watcher, [fake])
  })

  test('respawns automatically on an unexpected exit', async () => {
    const ctrl = makeSpawner()
    const onChildStarted = vi.fn()
    const onUnexpectedExit = vi.fn()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted,
      onUnexpectedExit,
    })
    await watcher.firstSpawn
    ctrl.fakes[0]!.simulateExit(1, null)
    await flush()
    expect(ctrl.fakes).toHaveLength(2)
    expect(onChildStarted).toHaveBeenCalledTimes(2)
    expect(onUnexpectedExit).toHaveBeenCalledTimes(1)
    expect(onUnexpectedExit).toHaveBeenCalledWith('exited with code=1 signal=null', {
      exceedsCrashLimit: false,
      exitError: null,
    })
    expect(watcher.respawnSuspended).toBe(false)
    await teardown(watcher, ctrl.fakes)
  })

  test("forwards the child's exitError to onUnexpectedExit on async 'error'", async () => {
    const ctrl = makeSpawner()
    const onUnexpectedExit = vi.fn(() => false as const)
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit,
    })
    await watcher.firstSpawn
    const err = enoent()
    ctrl.fakes[0]!.simulateError(err)
    await flush()
    expect(onUnexpectedExit).toHaveBeenCalledTimes(1)
    expect(onUnexpectedExit).toHaveBeenCalledWith(expect.stringContaining('ENOENT'), {
      exceedsCrashLimit: false,
      exitError: err,
    })
    await watcher.close()
  })

  test('returning false from onUnexpectedExit suspends respawn', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit: () => false,
    })
    await watcher.firstSpawn
    ctrl.fakes[0]!.simulateExit(1, null)
    await flush()
    expect(ctrl.fakes).toHaveLength(1)
    expect(watcher.respawnSuspended).toBe(true)
    await watcher.close()
  })

  test('returning true from onUnexpectedExit forces respawn even past the crash limit', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit: () => true,
      crashLoopLimit: { maxCrashes: 2, windowMs: 60_000 },
    })
    await watcher.firstSpawn
    for (let i = 0; i < 5; i++) {
      ctrl.fakes[i]!.simulateExit(1, null)
      await flush()
    }
    expect(ctrl.fakes).toHaveLength(6)
    expect(watcher.respawnSuspended).toBe(false)
    await teardown(watcher, ctrl.fakes)
  })

  test('crash-loop guard sets exceedsCrashLimit and auto-suspends with undefined return', async () => {
    const ctrl = makeSpawner()
    const onUnexpectedExit = vi.fn(() => undefined)
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit,
      crashLoopLimit: { maxCrashes: 3, windowMs: 60_000 },
    })
    await watcher.firstSpawn

    ctrl.fakes[0]!.simulateExit(1, null)
    await flush()
    ctrl.fakes[1]!.simulateExit(1, null)
    await flush()
    expect(onUnexpectedExit).toHaveBeenLastCalledWith(expect.any(String), {
      exceedsCrashLimit: false,
      exitError: null,
    })
    expect(ctrl.fakes).toHaveLength(3)

    ctrl.fakes[2]!.simulateExit(1, null)
    await flush()
    expect(onUnexpectedExit).toHaveBeenLastCalledWith(expect.any(String), {
      exceedsCrashLimit: true,
      exitError: null,
    })
    expect(ctrl.fakes).toHaveLength(3)
    expect(watcher.respawnSuspended).toBe(true)
    await watcher.close()
  })

  test('crash-loop window slides — exits older than windowMs do not count', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit: () => undefined,
      crashLoopLimit: { maxCrashes: 3, windowMs: 1_000 },
    })
    await watcher.firstSpawn

    ctrl.fakes[0]!.simulateExit(1, null)
    await flush()
    ctrl.fakes[1]!.simulateExit(1, null)
    await flush()

    await vi.advanceTimersByTimeAsync(1_500)

    ctrl.fakes[2]!.simulateExit(1, null)
    await flush()
    expect(watcher.respawnSuspended).toBe(false)
    expect(ctrl.fakes).toHaveLength(4)
    await teardown(watcher, ctrl.fakes)
  })

  test('crash buffer is preserved across manual respawn', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit: () => undefined,
      crashLoopLimit: { maxCrashes: 2, windowMs: 60_000 },
    })
    await watcher.firstSpawn

    ctrl.fakes[0]!.simulateExit(1, null)
    await flush()
    ctrl.fakes[1]!.simulateExit(1, null)
    await flush()
    expect(watcher.respawnSuspended).toBe(true)

    await watcher.respawn()
    expect(watcher.respawnSuspended).toBe(false)
    expect(ctrl.fakes).toHaveLength(3)

    ctrl.fakes[2]!.simulateExit(1, null)
    await flush()
    expect(watcher.respawnSuspended).toBe(true)
    await watcher.close()
  })

  test('manual respawn terminates the live child and spawns a new one', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, defaultOptions())
    const first = await watcher.firstSpawn
    const respawnPromise = watcher.respawn()
    expect(ctrl.fakes[0]!.killCalls).toEqual(['SIGTERM'])
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await respawnPromise
    expect(first.alive).toBe(false)
    expect(ctrl.fakes).toHaveLength(2)
    expect(watcher.current).not.toBe(first)
    expect(watcher.current?.alive).toBe(true)
    await teardown(watcher, ctrl.fakes)
  })

  test('manual respawn does not trigger onUnexpectedExit', async () => {
    const ctrl = makeSpawner()
    const onUnexpectedExit = vi.fn()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit,
    })
    await watcher.firstSpawn
    const respawnPromise = watcher.respawn()
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await respawnPromise
    await flush()
    expect(onUnexpectedExit).not.toHaveBeenCalled()
    await teardown(watcher, ctrl.fakes)
  })

  test('subsequent spawner failures route through onUnexpectedExit', async () => {
    const ctrl = makeSpawner()
    const onUnexpectedExit = vi.fn(() => false as const)
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit,
    })
    await watcher.firstSpawn

    ctrl.failNext(new Error('respawn boom'))
    const respawnPromise = watcher.respawn()
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await respawnPromise
    await flush()

    expect(onUnexpectedExit).toHaveBeenCalledTimes(1)
    expect(onUnexpectedExit).toHaveBeenCalledWith('spawn failed: respawn boom', {
      exceedsCrashLimit: false,
      exitError: expect.objectContaining({ message: 'respawn boom' }),
    })
    expect(watcher.respawnSuspended).toBe(true)
  })

  test('close prevents future respawns and SIGTERMs the live child', async () => {
    const ctrl = makeSpawner()
    const onUnexpectedExit = vi.fn()
    const watcher = new WatchedChildProcess(ctrl.spawner, {
      onChildStarted: () => undefined,
      onUnexpectedExit,
    })
    await watcher.firstSpawn
    const closePromise = watcher.close()
    expect(ctrl.fakes[0]!.killCalls).toEqual(['SIGTERM'])
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await closePromise
    await flush()
    expect(onUnexpectedExit).not.toHaveBeenCalled()
    expect(ctrl.fakes).toHaveLength(1)
  })

  test('close is idempotent', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, defaultOptions())
    await watcher.firstSpawn
    const first = watcher.close()
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await first
    await watcher.close()
    expect(ctrl.fakes).toHaveLength(1)
  })

  test('respawn after close is a no-op', async () => {
    const ctrl = makeSpawner()
    const watcher = new WatchedChildProcess(ctrl.spawner, defaultOptions())
    await watcher.firstSpawn
    const closePromise = watcher.close()
    ctrl.fakes[0]!.simulateExit(0, 'SIGTERM')
    await closePromise
    await watcher.respawn()
    expect(ctrl.fakes).toHaveLength(1)
  })

  test('close while the spawner is in flight kills the just-born child', async () => {
    let resolveSpawner: (value: ChildProcess) => void = () => {}
    const fake = new FakeChildProcess()
    const watcher = new WatchedChildProcess(
      () =>
        new Promise<ChildProcess>((resolve) => {
          resolveSpawner = resolve
        }),
      defaultOptions(),
    )
    await watcher.close()
    resolveSpawner(asChild(fake))
    await flush()
    expect(fake.killCalls).toEqual(['SIGTERM'])
  })
})
