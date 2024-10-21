/**
 * @file
 *
 * Provides set of hooks to work with offline status
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as eventCallback from '#/hooks/eventCallbackHooks'

/** Hook to get the offline status */
export function useOffline() {
  const isOnline = React.useSyncExternalStore(
    reactQuery.onlineManager.subscribe.bind(reactQuery.onlineManager),
    () => reactQuery.onlineManager.isOnline(),
    () => navigator.onLine,
  )

  return { isOffline: !isOnline }
}

/** Props for the {@link useOfflineChange} hook */
export interface UseOfflineChangeProps {
  readonly triggerImmediate?: boolean | 'if-offline' | 'if-online'
  readonly isDisabled?: boolean
}

/** Hook to subscribe to online/offline changes */
export function useOfflineChange(
  callback: (isOffline: boolean) => void,
  props: UseOfflineChangeProps = {},
) {
  const { triggerImmediate = 'if-offline', isDisabled = false } = props

  const lastCallValue = React.useRef<boolean | null>(null)
  const shouldTriggerCallback = React.useRef(false)
  const triggeredImmediateRef = React.useRef(isDisabled)

  const { isOffline } = useOffline()
  const isOnline = !isOffline

  const callbackEvent = eventCallback.useEventCallback((offline: boolean) => {
    if (isDisabled) {
      shouldTriggerCallback.current = true
    } else {
      if (lastCallValue.current !== offline) {
        callback(offline)
      }

      shouldTriggerCallback.current = false
      lastCallValue.current = offline
    }
  })

  if (!triggeredImmediateRef.current) {
    triggeredImmediateRef.current = true

    if (triggerImmediate === 'if-offline' && isOffline) {
      callbackEvent(isOffline)
    } else if (triggerImmediate === 'if-online' && isOnline) {
      callbackEvent(false)
    } else if (triggerImmediate === true) {
      callbackEvent(isOffline)
    }
  }

  React.useEffect(
    () =>
      reactQuery.onlineManager.subscribe((online) => {
        callbackEvent(!online)
      }),
    [callbackEvent],
  )

  React.useEffect(() => {
    if (shouldTriggerCallback.current && !isDisabled && lastCallValue.current != null) {
      callbackEvent(lastCallValue.current)
    }
  }, [callbackEvent, isDisabled])
}
