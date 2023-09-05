import {} from '@vue/reactivity'

import { computed, isRef, shallowRef, type Ref, type WatchSource, watchEffect } from 'vue'

/** Cast watch source to an observable ref. */
export function watchSourceToRef<T>(src: WatchSource<T>): Ref<T> {
  return isRef(src) ? src : computed(src)
}

/** Get the value behind a watch source at the current time. */
export function evalWatchSource<T>(src: WatchSource<T>): T {
  return isRef(src) ? src.value : src()
}

type UnwrapPromise<T> = T extends Promise<infer U> ? U : T
type RunFn = <T>(p: T) => Generator<RunToken, UnwrapPromise<T>>

declare const runBrand: unique symbol
type RunToken = { [runBrand]: never }

export function cancellableAsyncComputed<T>(
  generator: (run: RunFn) => Generator<RunToken, T, void>,
  initial = undefined,
): Ref<T | typeof initial> {
  const value = shallowRef<T>()
  const gen = generator(run)

  function run<T>(p: T): Generator<never, UnwrapPromise<T>, void> {
    throw 'TODO'
    // watchEffect((onCleanup) => {
    //   const nextRunnable = gen.next(p)
    //   if (next.done) {
    //     value.value = next.value
    //   }
    //   onCleanup(() => {
    //     if (!next.done) {
    //       gen.return()
    //     }
    //   })
    // })
  }

  // declare const brand: unique symbol
  // type Runnable<T> = T & { [brand]: never }

  // function run<T>(p: Promise<T>): Generator<never, Runnable<T>> {}

  // watchEffect(async (onCleanup) => {})

  return value
}

const x = cancellableAsyncComputed(function* (run) {
  const x = yield* run(5)
  const y = yield* run(Promise.resolve('bla'))
  const z = yield* someGen()
})

function* someGen() {}
