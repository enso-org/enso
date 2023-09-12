import type { Opt } from './opt'

/** An array that has at least one element present at all times. */
export type NonEmptyArray<T> = [T, ...T[]]

export function findIndexOpt<T>(arr: T[], pred: (elem: T) => boolean): Opt<number> {
  const index = arr.findIndex(pred)
  return index >= 0 ? index : null
}
