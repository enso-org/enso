import { nop } from 'lib0/function'
import {
  callWithErrorHandling,
  computed,
  effect,
  effectScope,
  isRef,
  queuePostFlushCb,
  type Ref,
  type WatchSource,
} from 'vue'

/** Cast watch source to an observable ref. */
export function watchSourceToRef<T>(src: WatchSource<T>): Ref<T> {
  return isRef(src) ? src : computed(src)
}

/** Get the value behind a watch source at the current time. */
export function evalWatchSource<T>(src: WatchSource<T>): T {
  return isRef(src) ? src.value : src()
}

export type OnCleanup = (fn: () => void) => void
export type StopEffect = () => void

/**
 * A set of effects that will defer their re-execution until an explicit flush. This is useful for
 * implementing incremental updates to an external data structure, while delaying the update logic
 * until next time that data structure is queried.
 */
export class LazySyncEffectSet {
  _dirtyRunners = new Set<() => void>()
  _scope = effectScope()
  _boundFlush = this.flush.bind(this)

  /**
   * Add an effect to the lazy set. The effect will run once immediately, and any subsequent runs
   * will be delayed until the next flush. Only effects that were notified about a dependency change
   * will be re-run on flush.
   *
   * Returns a function that can be used to manually stop the effect.
   */
  lazyEffect(fn: (onCleanup: OnCleanup) => void): StopEffect {
    return (
      this._scope.run(() => {
        let cleanup: (() => void) | null = null
        const callCleanup = () => {
          if (cleanup != null) {
            callWithErrorHandling(cleanup, null, 4 /* ErrorCodes.WATCH_CLEANUP */)
            cleanup = null
          }
        }
        function onCleanup(fn: () => void) {
          cleanup = fn
        }

        const runner = effect(
          () => {
            callCleanup()
            fn(onCleanup)
          },
          {
            lazy: true,
            scheduler: () => {
              if (this._dirtyRunners.size === 0) queuePostFlushCb(this._boundFlush)
              this._dirtyRunners.add(runner)
            },
            onStop: () => {
              this._dirtyRunners.delete(runner)
              callCleanup()
            },
          },
        )
        runner.effect.scheduler?.()
        return () => runner.effect.stop()
      }) ?? nop
    )
  }

  /**
   * Run all effects that are currently dirty. In case any effect causes other effects to become
   * dirty, they will be rerun during the same flush.
   */
  flush() {
    while (this._dirtyRunners.size !== 0) {
      const runners = [...this._dirtyRunners]
      this._dirtyRunners.clear()
      for (let i = 0; i < runners.length; ++i) runners[i]!()
    }
  }

  // Immediately stops all effects and clears the dirty set.
  stop() {
    this._scope.stop()
  }
}
