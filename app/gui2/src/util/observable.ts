import type { ObservableV2 } from 'lib0/observable'

export type Events<O extends ObservableV2<any>> = O extends ObservableV2<infer E> ? E : never

export function waitForEvent<O extends ObservableV2<any>, NAME extends string>(
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
  interface ObservableV2<EVENTS extends { [key: string]: (...arg0: any[]) => void }> {
    [EVENTS_BRAND]: EVENTS
  }
}
