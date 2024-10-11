/** @file `useEvent` shim. */
import { useCallback } from 'react'

import { useSyncRef } from '#/hooks/syncRefHooks'

/**
 * `useEvent` shim.
 * @see https://github.com/reactjs/rfcs/pull/220
 * @see https://github.com/reactjs/rfcs/blob/useevent/text/0000-useevent.md#internal-implementation
 */
export function useEventCallback<Func extends (...args: never[]) => unknown>(callback: Func) {
  const callbackRef = useSyncRef(callback)

  // Make sure that the value of `this` provided for the call to fn is not `ref`
  // This type assertion is safe, because it's a transparent wrapper around the original callback
  // we mute react-hooks/exhaustive-deps because we don't need to update the callback when the callbackRef changes(it never does)
  // eslint-disable-next-line react-hooks/exhaustive-deps, no-restricted-syntax
  return useCallback(((...args) => callbackRef.current.apply(undefined, args)) as Func, [])
}
