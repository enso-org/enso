/** @file */
import * as React from 'react'

import * as eventCallback from './eventCallbackHooks'

/** Calls callback when component is unmounted. */
export function useUnmount(callback: () => void) {
  // by using `useEventCallback` we can ensure that the callback is stable
  const callbackEvent = eventCallback.useEventCallback(callback)

  React.useEffect(() => callbackEvent, [callbackEvent])
}
