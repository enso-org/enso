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
import { ApplicationConfigValue } from '@/util/config'
import { QueryClientProvider } from '@tanstack/react-query'
import { QueryClient } from '@tanstack/vue-query'
import { IS_DEV_MODE, isOnElectron, isOnLinux } from 'enso-common/src/detect'
import { StrictMode } from 'react'
import invariant from 'tiny-invariant'

interface ReactRootProps {
  config: ApplicationConfigValue
  queryClient: QueryClient
  classSet: Map<string, number>
  onAuthenticated: (accessToken: string | null) => void
}

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

/**
 * A component gathering all views written currently in React with necessary contexts.
 */
export default function ReactRoot(props: ReactRootProps) {
  const { config, queryClient, onAuthenticated } = props

  const httpClient = new HttpClient()
  const supportsDeepLinks = !IS_DEV_MODE && !isOnLinux() && isOnElectron()
  const portalRoot = document.querySelector('#enso-portal-root')
  const shouldUseAuthentication = config.authentication.enabled
  const projectManagerUrl =
    (config.engine.projectManagerUrl || resolveEnvUrl($config.PROJECT_MANAGER_URL)) ?? null
  const ydocUrl = (config.engine.ydocUrl || resolveEnvUrl($config.YDOC_SERVER_URL)) ?? null
  const initialProjectName = config.startup.project || null
  invariant(portalRoot, 'PortalRoot element not found')
  const isCloudBuild = $config.CLOUD_BUILD === 'true'

  return (
    <StrictMode>
      <QueryClientProvider client={queryClient}>
        <ErrorBoundary>
          <UIProviders locale="en-US" portalRoot={portalRoot}>
            <Suspense fallback={<LoadingScreen />}>
              <OfflineNotificationManager>
                <LoggerProvider logger={console}>
                  <HttpClientProvider httpClient={httpClient}>
                    <App
                      supportsDeepLinks={supportsDeepLinks}
                      supportsLocalBackend={!isCloudBuild}
                      isAuthenticationDisabled={!shouldUseAuthentication}
                      projectManagerUrl={projectManagerUrl}
                      ydocUrl={ydocUrl}
                      initialProjectName={initialProjectName}
                      onAuthenticated={onAuthenticated}
                    />
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
