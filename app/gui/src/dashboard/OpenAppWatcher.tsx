/**
 * @file
 *
 * This file contains the OpenAppWatcher component.
 *
 * This component logs the user opening and closing the app.
 * It uses the remote backend to log the events.
 * Users can see these logs in Activity Log.
 */

import * as React from 'react'

import { useBackends } from '$/providers/react'
import { useMutationCallback } from './utilities/tanstackQuery'

/**
 * This component logs the user opening and closing the app.
 * It uses the remote backend to log the events.
 */
export function OpenAppWatcher({ children }: React.PropsWithChildren) {
  const { remoteBackend } = useBackends()

  const logUserOpenApp = useMutationCallback({
    mutationFn: () => remoteBackend.logEvent('open_app'),
  })

  const logUserCloseApp = useMutationCallback({
    mutationFn: () => remoteBackend.logEvent('close_app'),
  })

  React.useEffect(() => {
    void logUserOpenApp()
  }, [logUserOpenApp])

  React.useEffect(
    () => () => {
      void logUserCloseApp()
    },
    [logUserCloseApp],
  )

  React.useEffect(() => {
    const logCloseEvent = () => {
      void logUserCloseApp()
    }

    window.addEventListener('beforeunload', logCloseEvent)

    return () => {
      window.removeEventListener('beforeunload', logCloseEvent)
    }
  }, [logUserCloseApp])

  return <>{children}</>
}
