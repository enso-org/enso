/** @file Functions for manipulating Vue reactive objects. */

import { defaultEquality } from '@/util/equals'
import { debouncedWatch } from '@vueuse/core'
import { nop } from 'lib0/function'
import type {
  ComputedRef,
  // (it is used in docs)
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  DebuggerOptions,
  DeepReadonly,
  MaybeRefOrGetter,
  ReactiveEffectOptions,
  ReactiveEffectRunner,
  Ref,
  WatchSource,
  WatchStopHandle,
  WritableComputedRef,
} from 'vue'
import {
  callWithErrorHandling,
  computed,
  effect,
  effectScope,
  isRef,
  queuePostFlushCb,
  reactive,
  ReactiveEffect,
  shallowReactive,
  shallowRef,
  toRaw,
  toValue,
  watch,
} from 'vue'

/** Cast watch source to an observable ref. */
export function watchSourceToRef<T>(src: WatchSource<T>): Ref<T> {
  return isRef(src) ? src : computed(src)
}

/** Get the value behind a watch source at the current time. */
export function evalWatchSource<T>(src: WatchSource<T>): T {
  return isRef(src) ? src.value : src()
}

/** Create a `ReactiveEffect`. This is similar to the `effect` function, but doesn't immediately run the created effect. */
export function lazyEffect<T = any>(
  fn: () => T,
  options?: ReactiveEffectOptions,
): ReactiveEffectRunner<T> {
  if ((fn as ReactiveEffectRunner).effect instanceof ReactiveEffect) {
    fn = (fn as ReactiveEffectRunner).effect.fn
  }

  const e = new ReactiveEffect(fn)
  if (options) {
    Object.assign(e, options)
  }
  const runner = e.run.bind(e) as ReactiveEffectRunner
  runner.effect = e
  return runner
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
  _boundFlush = this.flush.bind(this)

  /** TODO: Add docs */
  constructor(private _scope = effectScope()) {}

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
            const tmpCleanup = cleanup
            cleanup = null
            callWithErrorHandling(tmpCleanup, null, 4 /* ErrorCodes.WATCH_CLEANUP */)
          }
        }
        function onCleanup(fn: () => void) {
          cleanup = fn
        }

        const runner = lazyEffect(
          () => {
            callCleanup()
            fn(onCleanup)
          },
          {
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
  /** TODO: Add docs */
  stop() {
    this._scope.stop()
  }
}

/**
 * Supports dynamically creating watchers that will be scheduled with the same priority as a watcher created where
 * `useWatchContext` is called.
 *
 * Watchers created during a component's `setup` are scheduled according to the component hierarchy; all watchers of a
 * parent component are run before any watchers of a child component. If a watcher isn't created during `setup`, e.g.
 * because it is created in a `watchEffect` handler, it will not have any associated instance, and will run after all
 * other pre-flush jobs, including all jobs associated with descendants.
 *
 * Calling this function during a component's `setup` captures the component instance's watch context, and provides an
 * API for dynamically creating watchers that will be scheduled as if they'd been created directly by the component.
 */
export function useWatchContext(): { watchEffect: (f: () => void) => WatchStopHandle } {
  const queued = new Set<object>()
  const jobs = reactive(new Array<() => void>())
  watch(jobs, () => {
    while (jobs.length > 0) {
      const job = jobs.pop()!
      // Do not run scheduled job if it's stopped. It's consistent with vue's "watchEffect" (checked in tests)
      if (queued.delete(job)) {
        job()
      }
    }
  })
  function watchEffect(f: () => void) {
    const runner = effect(f, {
      scheduler: () => {
        if (!queued.has(runner)) {
          jobs.push(runner)
          queued.add(runner)
        }
      },
      allowRecurse: true,
    })
    return () => {
      runner.effect.stop()
      queued.delete(runner)
    }
  }
  return { watchEffect }
}

/**
 * Create a ref that is updated whenever the given function's return value changes. Similar to
 * `computed`, but differs in significant ways:
 * - The dependencies of the `getter` will not be propagated to any effects that access the result.
 * - The `getter` will run even if the ref is not actively observed by any effects (i.e. it is
 * effectively an **always-active watcher**),
 * - The `ref` will only be updated only when the derived value actually changed (as determined by
 *   the `equalFn` equality test).
 *
 * This is most useful for derived values that are accessed often and recompute frequently, but the
 * actual derived result changes very rarely. When in doubt, use `computed` instead.
 */
export function cachedGetter<T>(
  getter: () => T,
  equalFn: (a: T, b: T) => boolean = defaultEquality,
): Ref<T> {
  const valueRef = shallowRef<T>(getter())
  watch(
    getter,
    (newValue) => {
      const oldValue = valueRef.value
      if (!equalFn(oldValue, newValue)) valueRef.value = newValue
    },
    { flush: 'sync' },
  )

  return valueRef
}

/**
 * Same as `cachedGetter`, except that any changes will be not applied immediately, but only after
 * the timer set for `debounce` milliseconds will expire. If any further update arrives in that
 * time, the timer is restarted.
 */
export function debouncedGetter<T>(
  getter: WatchSource<T>,
  debounce: number,
  equalFn: (a: T, b: T) => boolean = defaultEquality,
): Ref<T> {
  const valueRef = shallowRef<T>(toValue(getter))
  debouncedWatch(
    getter,
    (newValue) => {
      const oldValue = valueRef.value
      if (!equalFn(oldValue, newValue)) valueRef.value = newValue
    },
    { debounce },
  )
  return valueRef
}

/** Update `target` to have the same entries as `newState`. */
export function syncSet<T>(target: Set<T>, newState: Readonly<Set<T>>) {
  syncSetDiff(target, target, newState)
}

/**
 * Apply differences from `oldState` to `newState` to target.
 *
 * This can be used to update a reactive `target` without incurring a reactive dependency on the object being mutated,
 * by passing the underlying raw set as `oldState`.
 */
export function syncSetDiff<T>(
  target: Set<T>,
  oldState: DeepReadonly<Set<T>> | Readonly<Set<T>>,
  newState: Readonly<Set<T>>,
) {
  for (const oldKey of oldState) if (!newState.has(oldKey as T)) target.delete(oldKey as T)
  for (const newKey of newState) if (!oldState.has(newKey as any)) target.add(newKey)
}

/** Type of the parameter of `toValue`. */
export type ToValue<T> = MaybeRefOrGetter<T> | ComputedRef<T>

/**
 * A writable proxy computed value that reads a fallback value in case the base is `undefined`.
 * Useful for cases where we have a user-overridable behavior with a computed default.
 */
export function computedFallback<T>(
  base: Ref<T | undefined>,
  fallback: () => T,
): WritableComputedRef<T> {
  return computed({
    get: () => base.value ?? fallback(),
    set: (val: T) => (base.value = val),
  })
}

/**
 * Given a "raw" getter and setter, returns a writable-computed that buffers `set` operations.
 *
 * When the setter of the returned ref is invoked, the raw setter will be called during the next callback flush if and
 * only if the most recently set value does not compare strictly-equal to the current value (read from the raw getter).
 *
 * The getter of the returned ref immediately reflects the value of any pending write.
 */
export function useBufferedWritable<T>(raw: {
  get: ToValue<T>
  set: (value: T) => void
}): WritableComputedRef<T> {
  const pendingWrite = shallowRef<{ pending: T }>()
  watch(pendingWrite, () => {
    if (pendingWrite.value) {
      if (pendingWrite.value.pending !== toValue(raw.get)) {
        raw.set(pendingWrite.value.pending)
      }
      pendingWrite.value = undefined
    }
  })
  return computed({
    get: () => (pendingWrite.value ? pendingWrite.value.pending : toValue(raw.get)),
    set: (value: T) => (pendingWrite.value = { pending: value }),
  })
}

declare const brandNonReactiveView: unique symbol

/** Marks a readonly non-reactive view of data that may elsewhere be used reactively. */
export type NonReactiveView<T> = DeepReadonly<T> & { [brandNonReactiveView]: never }

/** Returns a readonly non-reactive view of a potentially-reactive value. */
export function nonReactiveView<T>(value: T): NonReactiveView<T> {
  return toRaw(value) as NonReactiveView<T>
}

/**
 * Given a non-reactive view of a value, return a reactive view.
 *
 * The type parameter can be specified to cast away the `DeepReadonly` added when converting to a `NonReactiveView`.
 * Note that if the specified type is not exactly the type of the value that was cast to `NonReactiveView`, this could
 * cast away `readonly` attributes that were present in the original type.
 */
export function resumeReactivity<T>(view: NonReactiveView<DeepReadonly<T>>): T {
  return reactive(view) as T
}

/**
 * Given a non-reactive view of a value, return a shallowly-reactive view.
 *
 *
 * The type parameter can be specified to cast away the `DeepReadonly` added when converting to a `NonReactiveView`.
 * Note that if the specified type is not exactly the type of the value that was cast to `NonReactiveView`, this could
 * cast away `readonly` attributes that were present in the original type.
 */
export function resumeShallowReactivity<T>(view: NonReactiveView<DeepReadonly<T>>): T {
  return shallowReactive(view) as T
}

/**
 * Return a writable computed that reads/writes either `left` or `right` depending on the value of `select`
 *
 * `true` means `left`, `false` means `right`.
 */
export function useSelectRef<T>(
  select: ToValue<boolean>,
  left: Ref<T>,
  right: Ref<T>,
): WritableComputedRef<T> {
  return computed({
    get() {
      return toValue(select) ? left.value : right.value
    },
    set(v: T) {
      if (toValue(select)) {
        left.value = v
      } else {
        right.value = v
      }
    },
  })
}

/**
 * Log any access to `anyRef`’s `value` property.
 *
 * {@link computed} and {@link watch} allow you to set custom
 * {@link DebuggerOptions.onTrack} and {@link DebuggerOptions.onTrigger} callbacks,
 * but they only allow tracking dependencies and recomputations, not access to the value itself.
 *
 * This function solves this issue by wrapping the `value` property of a ref with a custom getter.
 * You can also set a debugger breakpoint in the getter to precisely track the callstack of each access.
 */
export function debugAccess(anyRef: Ref<any>, name: string) {
  const originalGetter = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(anyRef), 'value')

  Object.defineProperty(anyRef, 'value', {
    ...originalGetter,
    get: () => {
      console.log(`Accessing ${name}`)
      return originalGetter?.get?.call(anyRef)
    },
  })
}
