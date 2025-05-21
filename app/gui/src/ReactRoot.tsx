/** @file A file containing setup for React part of application. */

import App from '#/App.tsx'
import { ReactQueryDevtools } from '#/components/Devtools'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { OfflineNotificationManager } from '#/components/OfflineNotificationManager'
import { Suspense } from '#/components/Suspense'
import UIProviders from '#/components/UIProviders'
import { useMount } from '#/hooks/mountHooks'
import LoadingScreen from '#/pages/authentication/LoadingScreen'
import { useSetFeatureFlag } from '#/providers/FeatureFlagsProvider'
import LoggerProvider from '#/providers/LoggerProvider'
import { QueryClientProvider } from '@tanstack/react-query'
import { QueryClient } from '@tanstack/vue-query'
import { PropsWithChildren, StrictMode } from 'react'
import invariant from 'tiny-invariant'

interface ReactRootProps {
  queryClient: QueryClient
  onAuthenticated: (accessToken: string | null) => void
}

/**
 * A component gathering all views written currently in React with necessary contexts.
 */
export default function ReactRoot(props: PropsWithChildren<ReactRootProps>) {
  const { queryClient, onAuthenticated, children } = props

  const appRoot = document.querySelector('#enso-app')
  invariant(appRoot instanceof HTMLElement, 'AppRoot element not found')

  const portalRoot = document.querySelector('#enso-portal-root')
  invariant(portalRoot instanceof HTMLElement, 'PortalRoot element not found')

  const setFeatureFlag = useSetFeatureFlag()
  useMount(() => {
    if (typeof window !== 'undefined' && window.overrideFeatureFlags === undefined) {
      setFeatureFlag('enableLocalBackend', $config.CLOUD_BUILD !== 'true')
    }
  })

  return (
    <StrictMode>
      <QueryClientProvider client={queryClient}>
        <ErrorBoundary>
          <UIProviders locale="en-US" portalRoot={portalRoot} appRoot={appRoot}>
            <Suspense fallback={<LoadingScreen />}>
              <OfflineNotificationManager>
                <LoggerProvider logger={console}>
                  <App onAuthenticated={onAuthenticated}>{children}</App>
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
