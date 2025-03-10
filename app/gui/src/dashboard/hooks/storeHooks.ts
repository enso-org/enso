/**
 * @file
 *
 * This file contains hooks for using Zustand store with tearing transitions.
 */
import type { DispatchWithoutAction, Reducer, RefObject } from 'react'
import { useEffect, useReducer, useRef } from 'react'
import { type StoreApi } from 'zustand'
import { useStoreWithEqualityFn } from 'zustand/traditional'

import { objectEquality, refEquality, shallowEquality } from '../utilities/equalities'

/**
 * A type that allows to choose between different equality functions.
 */
export type AreEqual<T> = EqualityFunction<T> | EqualityFunctionName
/**
 * Custom equality function.
 */
export type EqualityFunction<T> = (a: T, b: T) => boolean
/**
 * Equality function name from a list of predefined ones.
 */
export type EqualityFunctionName = 'object' | 'shallow' | 'strict'

const EQUALITY_FUNCTIONS: Record<EqualityFunctionName, (a: unknown, b: unknown) => boolean> = {
  object: objectEquality,
  shallow: shallowEquality,
  strict: refEquality,
}

/** Options for the `useStore` hook. */
export interface UseStoreOptions<Slice> {
  /**
   * Adds support for React transitions.
   *
   * Use it with caution, as it may lead to inconsistent state during transitions.
   */
  readonly unsafeEnableTransition?: boolean
  /**
   * Specifies the equality function to use.
   * @default 'Object.is'
   */
  readonly areEqual?: AreEqual<Slice>
}

/**
 * A wrapper that allows to choose between tearing transition and standard Zustand store.
 *
 * # `options.unsafeEnableTransition` must not be changed during the component lifecycle.
 */
export function useStore<State, Slice>(
  store: StoreApi<State>,
  selector: (state: State) => Slice,
  options: UseStoreOptions<Slice> = {},
) {
  const { unsafeEnableTransition = false, areEqual } = options
  const prevUnsafeEnableTransition = useRef(unsafeEnableTransition)
  const equalityFunction = resolveAreEqual(areEqual)
  return useNonCompilableConditionalStore(
    store,
    selector,
    unsafeEnableTransition,
    equalityFunction,
    prevUnsafeEnableTransition,
  )
}

/** A hook that allows to use React transitions with Zustand store. */
export function useTearingTransitionStore<State, Slice>(
  store: StoreApi<State>,
  selector: (state: State) => Slice,
  areEqual: AreEqual<Slice> = 'shallow',
) {
  const state = store.getState()

  const equalityFunction = resolveAreEqual(areEqual)

  const [[sliceFromReducer, storeFromReducer], rerender] = useReducer<
    Reducer<
      readonly [Slice, StoreApi<State>, State],
      readonly [Slice, StoreApi<State>, State] | undefined
    >,
    undefined
  >(
    (prev, fromSelf) => {
      if (fromSelf) {
        return fromSelf
      }
      const nextState = store.getState()
      if (Object.is(prev[2], nextState) && prev[1] === store) {
        return prev
      }

      const nextSlice = selector(nextState)
      if (equalityFunction(prev[0], nextSlice) && prev[1] === store) {
        return prev
      }

      return [nextSlice, store, nextState]
    },
    undefined,
    () => [selector(state), store, state],
  )

  useEffect(() => {
    const unsubscribe = store.subscribe(() => {
      // eslint-disable-next-line no-restricted-syntax
      ;(rerender as DispatchWithoutAction)()
    })
    // eslint-disable-next-line no-restricted-syntax
    ;(rerender as DispatchWithoutAction)()
    return unsubscribe
  }, [store])

  if (storeFromReducer !== store) {
    const slice = selector(state)
    rerender([slice, store, state])
    return slice
  }

  return sliceFromReducer
}

/** Resolves the equality function. */
function resolveAreEqual<Slice>(areEqual: AreEqual<Slice> | null | undefined) {
  return (
    areEqual == null ? EQUALITY_FUNCTIONS.object
    : typeof areEqual === 'string' ? EQUALITY_FUNCTIONS[areEqual]
    : areEqual
  )
}

/**
 * Internal hook that isolates the conditional store logic from the `useStore` hook.
 * To enable compiler optimizations for the `useStore` hook.
 * @internal
 * @throws An error if the `unsafeEnableTransition` option is changed during the component lifecycle.
 */
function useNonCompilableConditionalStore<State, Slice>(
  store: StoreApi<State>,
  selector: (state: State) => Slice,
  unsafeEnableTransition: boolean,
  equalityFunction: EqualityFunction<Slice>,
  prevUnsafeEnableTransition: RefObject<boolean>,
) {
  /* eslint-disable react-compiler/react-compiler */
  /* eslint-disable react-hooks/rules-of-hooks */
  if (prevUnsafeEnableTransition.current !== unsafeEnableTransition) {
    throw new Error(
      'useStore shall not change the `unsafeEnableTransition` option during the component lifecycle',
    )
  }
  return unsafeEnableTransition ?
      useTearingTransitionStore(store, selector, equalityFunction)
    : useStoreWithEqualityFn(store, selector, equalityFunction)
  /* eslint-enable react-compiler/react-compiler */
  /* eslint-enable react-hooks/rules-of-hooks */
}
