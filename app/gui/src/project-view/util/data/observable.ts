/** Functions for querying {@link ObservableV2}s. */

import type { ObservableV2 } from 'lib0/observable'

/** Possible events which can be emitted by Observable. */
export type Events<O extends ObservableV2<any>> = O extends ObservableV2<infer E> ? E : never

/** Validate if a list of arguments covers all variants of the union type. */
export function stringUnionToArray<T>() {
  return <U extends NonEmptyArray<T>>(...elements: MustInclude<T, U>) => elements
}

type ValueOf<T> = T[keyof T]
type NonEmptyArray<T> = [T, ...T[]]
type MustInclude<T, U extends T[]> = [T] extends [ValueOf<U>] ? U : never

/**
 * Returns promise which will resolve on the next event. The promise will have the event's
 * payload.
 */
export function nextEvent<O extends ObservableV2<any>, NAME extends string>(
  observable: O,
  event: NAME,
): Promise<Parameters<Events<O>[NAME]>> {
  type Params = Parameters<Events<O>[NAME]>
  return new Promise<Params>((resolve) => {
    observable.once(event, (...args: Params) => {
      resolve(args)
    })
  })
}

declare const EVENTS_BRAND: unique symbol
declare module 'lib0/observable' {
  interface ObservableV2<EVENTS extends { [key in keyof EVENTS]: (...arg0: any[]) => void }> {
    [EVENTS_BRAND]: EVENTS
  }
}
