/** @file Liveness-tracking and watchdog wrappers around `node:child_process` instances. */

import type { ChildProcess } from 'node:child_process'
import { createDeferred, type Deferred } from './async.js'

const DEFAULT_TERMINATE_TIMEOUT_MS = 5_000

/**
 * Wraps a spawned {@link ChildProcess} and provides:
 * - reliable liveness tracking via the `'exit'` and `'error'` events (instead of
 *   `process.exitCode`, which stays `null` when the child terminates via signal — and
 *   never updates if the child fails to spawn at all),
 * - a graceful termination protocol with SIGKILL fallback after a timeout.
 */
export class ChildProcessHandle {
  private exitDeferred: Deferred<{ code: number | null; signal: NodeJS.Signals | null }> =
    createDeferred()
  private _alive = true
  private _exitReason: string | null = null
  private _exitError: NodeJS.ErrnoException | null = null

  /** Construct a handle for a child that has just been spawned. */
  constructor(public readonly child: ChildProcess) {
    child.once('exit', (code, signal) => {
      if (!this._alive) return
      this._alive = false
      this._exitReason = formatExitReason(code, signal)
      this.exitDeferred.resolve({ code, signal })
    })
    // 'error' fires when the child cannot be spawned (e.g. ENOENT on POSIX, where the spawner
    // returns a child object that emits 'error' asynchronously instead of throwing). 'exit' may
    // or may not follow; whichever fires first is treated as terminal.
    child.once('error', (err) => {
      if (!this._alive) return
      const errno = err as NodeJS.ErrnoException
      this._alive = false
      this._exitError = errno
      this._exitReason = formatErrorReason(errno)
      this.exitDeferred.resolve({ code: null, signal: null })
    })
  }

  /** True until the underlying process has emitted its `'exit'` or `'error'` event. */
  get alive(): boolean {
    return this._alive
  }

  /**
   * Set after the child has terminated to a human-readable description like
   * `"exited with code=1 signal=null"` or `"spawn error: ENOENT: spawn claude ENOENT"`.
   */
  get exitReason(): string | null {
    return this._exitReason
  }

  /**
   * The original `Error` from the child's `'error'` event (e.g. ENOENT when the executable was
   * not found). Null when the child exited via `'exit'` instead.
   */
  get exitError(): NodeJS.ErrnoException | null {
    return this._exitError
  }

  /** Resolves once the process has exited (immediately if it already has). */
  waitForExit(): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
    return this.exitDeferred.promise
  }

  /**
   * Send `signal` (default `SIGTERM`) and wait for the process to exit. If it has not exited
   * after `timeoutMs`, send `SIGKILL`. Resolves once `'exit'` has fired (or immediately if the
   * process was already dead).
   */
  async terminate({
    signal = 'SIGTERM' as NodeJS.Signals,
    timeoutMs = DEFAULT_TERMINATE_TIMEOUT_MS,
  }: { signal?: NodeJS.Signals; timeoutMs?: number } = {}): Promise<void> {
    if (!this._alive) return
    this.child.kill(signal)
    const killHandle = setTimeout(() => {
      if (this._alive) this.child.kill('SIGKILL')
    }, timeoutMs)
    try {
      await this.exitDeferred.promise
    } finally {
      clearTimeout(killHandle)
    }
  }
}

function formatExitReason(code: number | null, signal: NodeJS.Signals | null): string {
  return `exited with code=${code} signal=${signal ?? 'null'}`
}

function formatErrorReason(err: NodeJS.ErrnoException): string {
  return `spawn error: ${err.code ?? 'unknown'}: ${err.message}`
}

/** Information passed to {@link WatchedChildProcessOptions.onUnexpectedExit}. */
export interface UnexpectedExitInfo {
  /**
   * `true` when this crash, taken with previous recent ones, has hit the configured
   * {@link WatchedChildProcessOptions.crashLoopLimit}. With the limit unset, this is always
   * `false`. The default behavior of an `undefined` callback return depends on this flag:
   * suspend when `true`, respawn when `false`.
   */
  exceedsCrashLimit: boolean
  /**
   * The original error from the child's `'error'` event (e.g. ENOENT) or from a thrown spawner.
   * Null when the child exited normally via `'exit'`. Forwarded so the consumer can preserve
   * `.code` for downstream handling instead of parsing the `reason` string.
   */
  exitError: NodeJS.ErrnoException | null
}

/** Options for {@link WatchedChildProcess}. */
export interface WatchedChildProcessOptions {
  /**
   * Called immediately after a new child has been spawned. Wire up streams, kick off any
   * readiness check, etc. The handle's `alive` is `true` at the moment this fires.
   */
  onChildStarted: (handle: ChildProcessHandle) => void
  /**
   * Called when the child exits unexpectedly (i.e. when `close()` has not been called and the
   * watcher had not just been asked to `respawn()`). Runs synchronously inside the exit
   * handler — any state mutations take effect before the next spawn, so this is the right place
   * to reject pending work, prepare fresh deferreds, log, etc.
   *
   * Return `true` to force respawn, `false` to force suspension. Returning `undefined` defers
   * to the policy: respawn unless `info.exceedsCrashLimit` is `true`, in which case suspend.
   * After a suspension the consumer can call `respawn()` to resume.
   */
  onUnexpectedExit?: (reason: string, info: UnexpectedExitInfo) => boolean | undefined
  /**
   * Crash-loop guard. When set, the watcher tracks recent exits and reports
   * `info.exceedsCrashLimit = true` from the `maxCrashes`-th unexpected exit within `windowMs`
   * onwards. The recent-exits buffer is **not** cleared on respawn — that means a manual
   * `respawn()` followed by a quick crash trips the guard again immediately, instead of
   * granting an infinite retry stream.
   */
  crashLoopLimit?: { maxCrashes: number; windowMs: number }
}

/**
 * Owns a long-lived child process that is automatically respawned when it exits unexpectedly.
 * Designed for daemon-shaped subprocesses (a language server, a long-lived CLI) where the
 * consumer wants the child to stay running until explicitly closed.
 *
 * Crash-loop protection is built in via {@link WatchedChildProcessOptions.crashLoopLimit}. The
 * consumer can also override the per-exit decision in `onUnexpectedExit` (e.g. to never respawn
 * after a particular failure mode), or call `respawn()` manually to override a suspension.
 *
 * The first spawn is exposed separately as {@link firstSpawn} so callers can choose between
 * eager error propagation (await `firstSpawn` and surface failures during construction) and
 * lazy semantics (let `onUnexpectedExit` handle initial-spawn failures like later crashes).
 */
export class WatchedChildProcess {
  private currentHandle: ChildProcessHandle | null = null
  private closed = false
  private suppressNextAutoRespawn = false
  private firstSpawnSettled = false
  private firstSpawnDeferred: Deferred<ChildProcessHandle> = createDeferred()
  private respawnSuspended_ = false
  private exitTimes: number[] = []

  /** Spawn the first child and start watching. */
  constructor(
    private readonly spawner: () => ChildProcess | Promise<ChildProcess>,
    private readonly options: WatchedChildProcessOptions,
  ) {
    // Silence unhandled-rejection warnings for callers that don't await firstSpawn (e.g. when
    // a consumer prefers to surface initial-spawn errors via `onUnexpectedExit`).
    this.firstSpawnDeferred.promise.catch(() => undefined)
    void this.spawnNext()
  }

  /** The currently-running child handle, or `null` if no spawn has completed yet. */
  get current(): ChildProcessHandle | null {
    return this.currentHandle
  }

  /**
   * Resolves with the handle for the first successful spawn, or rejects if the first spawn
   * fails. Subsequent unexpected exits go through `onUnexpectedExit`, not this promise.
   */
  get firstSpawn(): Promise<ChildProcessHandle> {
    return this.firstSpawnDeferred.promise
  }

  /**
   * `true` while auto-respawn is suspended due to the crash-loop guard or an explicit
   * `false` return from `onUnexpectedExit`. Cleared by a successful manual `respawn()`.
   */
  get respawnSuspended(): boolean {
    return this.respawnSuspended_
  }

  /**
   * Manually trigger a respawn: terminate the current child (if alive) and spawn a new one.
   * Bypasses the crash-loop guard, so it can be used to recover from a suspended state.
   */
  async respawn(timeoutMs = DEFAULT_TERMINATE_TIMEOUT_MS): Promise<void> {
    if (this.closed) return
    this.respawnSuspended_ = false
    if (this.currentHandle?.alive) {
      this.suppressNextAutoRespawn = true
      await this.currentHandle.terminate({ timeoutMs })
    }
    await this.spawnNext()
  }

  /**
   * Permanently terminate. Suppresses respawn and SIGTERMs the current child, falling back to
   * SIGKILL after `timeoutMs`. Subsequent calls are no-ops.
   */
  async close(timeoutMs = DEFAULT_TERMINATE_TIMEOUT_MS): Promise<void> {
    this.closed = true
    await this.currentHandle?.terminate({ timeoutMs })
  }

  private async spawnNext(): Promise<void> {
    if (this.closed) return
    let child: ChildProcess
    try {
      child = await this.spawner()
    } catch (err) {
      if (!this.firstSpawnSettled) {
        this.firstSpawnSettled = true
        this.firstSpawnDeferred.reject(err)
        this.respawnSuspended_ = true
        return
      }
      const reason = `spawn failed: ${formatError(err)}`
      this.handleExit(reason, asErrno(err))
      return
    }
    if (this.closed) {
      // close() was called while the spawner was in flight. Kill the just-born child and bail.
      child.kill('SIGTERM')
      return
    }
    const handle = new ChildProcessHandle(child)
    this.currentHandle = handle
    if (!this.firstSpawnSettled) {
      this.firstSpawnSettled = true
      this.firstSpawnDeferred.resolve(handle)
    }
    this.options.onChildStarted(handle)
    void handle.waitForExit().then(() => {
      if (this.closed) return
      if (this.suppressNextAutoRespawn) {
        this.suppressNextAutoRespawn = false
        return
      }
      this.handleExit(handle.exitReason ?? 'unknown exit', handle.exitError)
    })
  }

  private handleExit(reason: string, exitError: NodeJS.ErrnoException | null): void {
    if (this.closed) return
    const exceedsCrashLimit = this.recordExitForCrashLimit()
    const decision = this.options.onUnexpectedExit?.(reason, { exceedsCrashLimit, exitError })
    const shouldRespawn = decision === undefined ? !exceedsCrashLimit : decision
    if (shouldRespawn) {
      this.respawnSuspended_ = false
      void this.spawnNext()
    } else {
      this.respawnSuspended_ = true
    }
  }

  private recordExitForCrashLimit(): boolean {
    const limit = this.options.crashLoopLimit
    if (limit == null) return false
    const now = Date.now()
    this.exitTimes = this.exitTimes.filter((t) => now - t <= limit.windowMs)
    this.exitTimes.push(now)
    return this.exitTimes.length >= limit.maxCrashes
  }
}

function formatError(err: unknown): string {
  if (err instanceof Error) return err.message
  return String(err ?? 'unknown error')
}

function asErrno(err: unknown): NodeJS.ErrnoException | null {
  return err instanceof Error ? (err as NodeJS.ErrnoException) : null
}
