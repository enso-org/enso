/** @file A helper function to properly type the parameters of a slice function. */
import type * as zustand from 'zustand'

// ===================
// === defineSlice ===
// ===================

/** A helper function to properly type the parameters of a slice function. */
export function defineSlice<T>() {
  return <Mos extends [zustand.StoreMutatorIdentifier, unknown][] = []>(
    initializer: zustand.StateCreator<T, [], Mos>
  ): zustand.StateCreator<T, [], Mos> => initializer
}
