/**
 * @file useEventCallback shim
 */

import * as React from 'react'

import * as syncRef from '#/hooks/syncRefHooks'

/**
 * useEvent shim.
 * @see https://github.com/reactjs/rfcs/pull/220
 * @see https://github.com/reactjs/rfcs/blob/useevent/text/0000-useevent.md#internal-implementation
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function useEventCallback<Func extends (...args: any[]) => unknown>(callback: Func) {
  const callbackRef = syncRef.useSyncRef(callback)

  // Make sure that the value of `this` provided for the call to fn is not `ref`
  // This type assertion is safe, because it's a transparent wrapper around the original callback
  // we mute react-hooks/exhaustive-deps because we don't need to update the callback when the callbackRef changes(it never does)
  // eslint-disable-next-line react-hooks/exhaustive-deps, no-restricted-syntax
  return React.useCallback(((...args) => callbackRef.current.apply(void 0, args)) as Func, [])
}
