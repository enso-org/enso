/**
 * @file useEventCallback shim from the latest React RFC.
 */

/* eslint-disable @typescript-eslint/no-explicit-any */
import * as React from 'react'

import * as syncRef from './syncRef'

/**
 * useEventCallback shim from the latest React RFC.
 * @see https://github.com/reactjs/rfcs/pull/220
 * @see https://github.com/reactjs/rfcs/blob/useevent/text/0000-useevent.md#internal-implementation
 */
export function useEventCallback<
  Func extends (...args: Args) => Result,
  Args extends Parameters<any> = Parameters<Func>,
  Result extends ReturnType<any> = ReturnType<Func>,
>(callback: Func): (...args: Args) => Result {
  const callbackRef = syncRef.useSyncRef(callback)

  // make sure that the value of `this` provided for the call to fn is not `ref`
  // eslint-disable-next-line no-restricted-syntax
  return React.useCallback((...args) => callbackRef.current.apply(void 0, args), [])
}
