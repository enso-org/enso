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

import * as backendProvider from '#/providers/BackendProvider'
import { useMutationCallback } from '../utilities/tanstackQuery'

/**
 * This component logs the user opening and closing the app.
 * It uses the remote backend to log the events.
 */
export function OpenAppWatcher({ children }: React.PropsWithChildren) {
  const remoteBackend = backendProvider.useRemoteBackend()

  const logUserOpenAppMutate = useMutationCallback({
    mutationFn: () => remoteBackend.logEvent('open_app'),
  })

  const logUserCloseAppMutate = useMutationCallback({
    mutationFn: () => remoteBackend.logEvent('close_app'),
  })

  React.useEffect(() => {
    void logUserOpenAppMutate()
  }, [logUserOpenAppMutate])

  React.useEffect(
    () => () => {
      void logUserCloseAppMutate()
    },
    [logUserCloseAppMutate],
  )

  React.useEffect(() => {
    const logCloseEvent = () => {
      void logUserCloseAppMutate()
    }

    window.addEventListener('beforeunload', logCloseEvent)

    return () => {
      window.removeEventListener('beforeunload', logCloseEvent)
    }
  }, [logUserCloseAppMutate])

  return <>{children}</>
}
