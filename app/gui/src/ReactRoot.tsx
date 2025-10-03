/** @file A file containing setup for React part of application. */

import App from '#/App.tsx'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { OfflineNotificationManager } from '#/components/OfflineNotificationManager'
import { Suspense } from '#/components/Suspense'
import UIProviders from '#/components/UIProviders'
import { useMount } from '#/hooks/mountHooks'
import LoadingScreen from '#/pages/authentication/LoadingScreen'
import LoggerProvider from '#/providers/LoggerProvider'
import { useBackends } from '$/providers/backends'
import { useSetFeatureFlag } from '$/providers/react/featureFlags'
import { QueryClientProvider } from '@tanstack/react-query'
import { QueryClient } from '@tanstack/vue-query'
import { StrictMode, type PropsWithChildren } from 'react'
import invariant from 'tiny-invariant'

interface ReactRootProps {
  queryClient: QueryClient
}

/**
 * A component gathering all views written currently in React with necessary contexts.
 */
export default function ReactRoot(props: PropsWithChildren<ReactRootProps>) {
  const { queryClient, children } = props

  const appRoot = document.querySelector('#enso-app')
  invariant(appRoot instanceof HTMLElement, 'AppRoot element not found')

  const portalRoot = document.querySelector('#enso-portal-root')
  invariant(portalRoot instanceof HTMLElement, 'PortalRoot element not found')

  const setFeatureFlag = useSetFeatureFlag()
  const { localBackend } = useBackends()
  useMount(() => {
    if (
      typeof window !== 'undefined' &&
      window.overrideFeatureFlags?.enableLocalBackend === undefined
    ) {
      setFeatureFlag('enableLocalBackend', localBackend != null)
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
                  <App>{children}</App>
                </LoggerProvider>
              </OfflineNotificationManager>
            </Suspense>
          </UIProviders>
        </ErrorBoundary>
      </QueryClientProvider>
    </StrictMode>
  )
}
