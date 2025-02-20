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

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'

import * as backendProvider from '#/providers/BackendProvider'

/**
 * This component logs the user opening and closing the app.
 * It uses the remote backend to log the events.
 */
export function OpenAppWatcher() {
  const context = router.useOutletContext()
  const remoteBackend = backendProvider.useRemoteBackend()

  const { mutate: logUserOpenAppMutate } = reactQuery.useMutation({
    mutationFn: () => remoteBackend.logEvent('open_app'),
  })

  const { mutate: logUserCloseAppMutate } = reactQuery.useMutation({
    mutationFn: () => remoteBackend.logEvent('close_app'),
  })

  React.useEffect(() => {
    logUserOpenAppMutate()
  }, [logUserOpenAppMutate])

  React.useEffect(
    () => () => {
      logUserCloseAppMutate()
    },
    [logUserCloseAppMutate],
  )

  React.useEffect(() => {
    const logCloseEvent = () => {
      logUserCloseAppMutate()
    }

    window.addEventListener('beforeunload', logCloseEvent)

    return () => {
      window.removeEventListener('beforeunload', logCloseEvent)
    }
  }, [logUserCloseAppMutate])

  return <router.Outlet context={context} />
}
