import React from 'react'

import RemoteBackend from '#/services/RemoteBackend'

import HttpClient from '#/utilities/HttpClient'

import * as authProvider from './AuthProvider'
import * as loggerProvider from './LoggerProvider'
import * as textProvider from './TextProvider'

// as `backend` will always be accessed using `useRemoteBackend`.
const RemoteBackendContext = React.createContext<RemoteBackend | null>(null)

/**
 * A React Provider that lets components get the current remote backend with current user session.
 */
export default function RemoteBackendProvider(props: React.PropsWithChildren) {
  const { session } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()

  const backend = React.useMemo(() => {
    if (session?.accessToken == null) return null
    const client = new HttpClient([['Authorization', `Bearer ${session.accessToken}`]])
    const backend = new RemoteBackend(client, logger, getText)
    return backend
  }, [session?.accessToken, logger, getText])

  return (
    <RemoteBackendContext.Provider value={backend}>{props.children}</RemoteBackendContext.Provider>
  )
}

/** Exposes a property to get the a remote backend using current session data. */
export function useRemoteBackend() {
  return React.useContext(RemoteBackendContext)
}
