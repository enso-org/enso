import { nop } from 'lib0/function'
import {
  callWithErrorHandling,
  computed,
  effect,
  effectScope,
  isRef,
  reactive,
  ref,
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
            scheduler: () => {
              this._dirtyRunners.add(runner)
            },
            onStop: () => {
              this._dirtyRunners.delete(runner)
              callCleanup()
            },
          },
        )
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

if (import.meta.vitest) {
  const { test, expect, vi } = import.meta.vitest

  test('LazySyncEffectSet', () => {
    const lazySet = new LazySyncEffectSet()

    const key1 = ref(0)
    const key2 = ref(100)
    const lazilyUpdatedMap = reactive(new Map<number, string>())

    let runCount = 0
    const stopA = lazySet.lazyEffect((onCleanup) => {
      const currentValue = key1.value
      lazilyUpdatedMap.set(currentValue, 'a' + runCount++)
      onCleanup(() => lazilyUpdatedMap.delete(currentValue))
    })

    lazySet.lazyEffect((onCleanup) => {
      const currentValue = key2.value
      lazilyUpdatedMap.set(currentValue, 'b' + runCount++)
      onCleanup(() => lazilyUpdatedMap.delete(currentValue))
    })

    // Dependant effect, notices when -1 key is inserted into the map by another effect.
    const cleanupSpy = vi.fn()
    lazySet.lazyEffect((onCleanup) => {
      const negOne = lazilyUpdatedMap.get(-1)
      if (negOne != null) {
        lazilyUpdatedMap.set(-2, `noticed ${negOne}!`)
        onCleanup(() => {
          cleanupSpy()
          lazilyUpdatedMap.delete(-2)
        })
      }
    })

    expect(lazilyUpdatedMap, 'The effects should run immediately after registration').toEqual(
      new Map([
        [0, 'a0'],
        [100, 'b1'],
      ]),
    )

    key1.value = 1
    expect(lazilyUpdatedMap, 'The effects should not perform any updates until flush').toEqual(
      new Map([
        [0, 'a0'],
        [100, 'b1'],
      ]),
    )

    key1.value = 2
    lazySet.flush()
    expect(
      lazilyUpdatedMap,
      'A cleanup and update should run on flush, but only for the updated key',
    ).toEqual(
      new Map([
        [2, 'a2'],
        [100, 'b1'],
      ]),
    )

    key1.value = 3
    key2.value = 103
    stopA()
    expect(
      lazilyUpdatedMap,
      'Stop should immediately trigger cleanup, but only for stopped effect',
    ).toEqual(new Map([[100, 'b1']]))

    lazySet.flush()
    expect(
      lazilyUpdatedMap,
      'Flush should trigger remaining updates, but not run the stopped effects',
    ).toEqual(new Map([[103, 'b3']]))

    key1.value = 4
    key2.value = 104
    lazySet.lazyEffect((onCleanup) => {
      const currentValue = key1.value
      lazilyUpdatedMap.set(currentValue, 'c' + runCount++)
      onCleanup(() => lazilyUpdatedMap.delete(currentValue))
    })
    expect(
      lazilyUpdatedMap,
      'Newly registered effect should run immediately, but not flush other effects',
    ).toEqual(
      new Map([
        [4, 'c4'],
        [103, 'b3'],
      ]),
    )

    key1.value = 5
    key2.value = 105
    lazySet.flush()
    expect(
      lazilyUpdatedMap,
      'Flush should trigger both effects when their dependencies change',
    ).toEqual(
      new Map([
        [105, 'b5'],
        [5, 'c6'],
      ]),
    )

    lazySet.flush()
    expect(lazilyUpdatedMap, 'Flush should have no effect when no dependencies changed').toEqual(
      new Map([
        [105, 'b5'],
        [5, 'c6'],
      ]),
    )

    key2.value = -1
    lazySet.flush()
    expect(
      lazilyUpdatedMap,
      'Effects depending on one another should run in the same flush',
    ).toEqual(
      new Map([
        [5, 'c6'],
        [-1, 'b7'],
        [-2, 'noticed b7!'],
      ]),
    )

    key2.value = 1
    lazySet.flush()
    expect(cleanupSpy).toHaveBeenCalledTimes(1)
    expect(lazilyUpdatedMap, 'Dependant effect is cleaned up.').toEqual(
      new Map([
        [1, 'b8'],
        [5, 'c6'],
      ]),
    )

    key2.value = 2
    lazySet.flush()
    key2.value = -1
    lazySet.flush()
    expect(cleanupSpy, 'Cleanup runs only once.').toHaveBeenCalledTimes(1)
  })
}
