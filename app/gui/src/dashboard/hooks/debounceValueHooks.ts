/**
 * @file
 *
 * This file contains the `useDebounceValue` hook.
 */
import * as debounceState from './debounceStateHooks'

/** Debounce a value. */
export function useDebounceValue<T>(value: T, delay: number, maxWait?: number) {
  const [debouncedValue, setDebouncedValue] = debounceState.useDebounceState(value, delay, maxWait)

  if (value !== debouncedValue) {
    setDebouncedValue(value)
  }

  return debouncedValue
}
