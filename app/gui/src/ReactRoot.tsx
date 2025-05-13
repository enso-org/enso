/** @file A file containing setup for React part of application. */

import App from '#/App.tsx'
import { ReactQueryDevtools } from '#/components/Devtools'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { OfflineNotificationManager } from '#/components/OfflineNotificationManager'
import { Suspense } from '#/components/Suspense'
import UIProviders from '#/components/UIProviders'
import LoadingScreen from '#/pages/authentication/LoadingScreen'
import { HttpClientProvider } from '#/providers/HttpClientProvider'
import LoggerProvider from '#/providers/LoggerProvider'
import HttpClient from '#/utilities/HttpClient'
import { QueryClientProvider } from '@tanstack/react-query'
import { QueryClient } from '@tanstack/vue-query'
import { IS_DEV_MODE, isOnElectron, isOnLinux } from 'enso-common/src/detect'
import { PropsWithChildren, StrictMode } from 'react'
import invariant from 'tiny-invariant'

interface ReactRootProps {
  queryClient: QueryClient
  classSet: Map<string, number>
  onAuthenticated: (accessToken: string | null) => void
}

function generateSessionID() {
  const sessionID = sessionStorage.getItem('sessionID')
  if (sessionID) {
    return sessionID
  }

  const newSessionID = crypto.randomUUID()
  sessionStorage.setItem('sessionID', newSessionID)
  return newSessionID
}

/**
 * A component gathering all views written currently in React with necessary contexts.
 */
export default function ReactRoot(props: PropsWithChildren<ReactRootProps>) {
  const { queryClient, onAuthenticated, children } = props

  const sessionID = generateSessionID()

  const httpClient = new HttpClient({
    'x-enso-ide-version': $config.VERSION ?? '',
    'x-enso-session-id': sessionID,
  })

  const supportsDeepLinks = !IS_DEV_MODE && !isOnLinux() && isOnElectron()

  const appRoot = document.querySelector('#enso-app')
  invariant(appRoot instanceof HTMLElement, 'AppRoot element not found')

  const portalRoot = document.querySelector('#enso-portal-root')
  invariant(portalRoot instanceof HTMLElement, 'PortalRoot element not found')

  const isCloudBuild = $config.CLOUD_BUILD === 'true'

  return (
    <StrictMode>
      <QueryClientProvider client={queryClient}>
        <ErrorBoundary>
          <UIProviders locale="en-US" portalRoot={portalRoot} appRoot={appRoot}>
            <Suspense fallback={<LoadingScreen />}>
              <OfflineNotificationManager>
                <LoggerProvider logger={console}>
                  <HttpClientProvider httpClient={httpClient}>
                    <App
                      supportsDeepLinks={supportsDeepLinks}
                      supportsLocalBackend={!isCloudBuild}
                      onAuthenticated={onAuthenticated}
                    >
                      {children}
                    </App>
                  </HttpClientProvider>
                </LoggerProvider>
              </OfflineNotificationManager>
            </Suspense>

            <ReactQueryDevtools />
          </UIProviders>
        </ErrorBoundary>
      </QueryClientProvider>
    </StrictMode>
  )
}
