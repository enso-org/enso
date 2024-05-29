/** @file The React provider for a `RemoteBackend` instance that is always configured to use active
 * session token. */
import * as React from 'react'

import invariant from 'tiny-invariant'

import * as authProvider from '#/providers/AuthProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as textProvider from '#/providers/TextProvider'

import RemoteBackend from '#/services/RemoteBackend'

import HttpClient from '#/utilities/HttpClient'

const RemoteBackendContext = React.createContext<RemoteBackend | null>(null)

/** A React Provider that lets components get the current remote backend with current user session. */
export default function RemoteBackendProvider(props: React.PropsWithChildren) {
  const { session } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()

  const backend = React.useMemo(() => {
    if (session?.accessToken == null) {
      return null
    } else {
      const client = new HttpClient([['Authorization', `Bearer ${session.accessToken}`]])
      return new RemoteBackend(client, logger, getText)
    }
  }, [session?.accessToken, logger, getText])

  return (
    <RemoteBackendContext.Provider value={backend}>{props.children}</RemoteBackendContext.Provider>
  )
}

/** Exposes a property to get the a remote backend using current session data. */
export function useRemoteBackend() {
  return React.useContext(RemoteBackendContext)
}

/** Exposes a property to get the a remote backend using current session data. */
export function useStrictRemoteBackend() {
  const backend = useRemoteBackend()
  invariant(backend != null, 'Remote backend not provided.')
  return backend
}
