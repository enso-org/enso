/** @file `useEvent` shim. */
import { useCallback } from 'react'
import { useSyncRef } from './syncRefHooks'

/**
 * `useEvent` shim.
 * @see https://github.com/reactjs/rfcs/pull/220
 * @see https://github.com/reactjs/rfcs/blob/useevent/text/0000-useevent.md#internal-implementation
 */
export function useEventCallback<Func extends (...args: never[]) => unknown>(
  callback: Func | false | null | undefined,
) {
  const callbackRef = useSyncRef(callback)

  return useCallback(
    // Make sure that the value of `this` provided for the call to fn is not `ref`
    // This type assertion is safe, because it's a transparent wrapper around the original callback
    // eslint-disable-next-line no-restricted-syntax
    function eventCallback(...args: Parameters<Func>) {
      if (typeof callbackRef.current === 'function') {
        return callbackRef.current(...args)
      }
    } as Func,
    [callbackRef],
  )
}
