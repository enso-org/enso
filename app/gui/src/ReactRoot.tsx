/** @file A file containing setup for React part of application. */

import App from '#/App.tsx'
import { ReactQueryDevtools } from '#/pages/Devtools'
import LoggerProvider from '#/providers/LoggerProvider'
import LoadingScreen from '$/authentication/LoadingScreen'
import { ErrorBoundary } from '$/react-components/ErrorBoundary'
import { OfflineNotificationManager } from '$/react-components/OfflineNotificationManager'
import { Suspense } from '$/react-components/Suspense'
import UIProviders from '$/react-components/UIProviders'
import { QueryClientProvider } from '@tanstack/react-query'
import { QueryClient } from '@tanstack/vue-query'
import { PropsWithChildren, StrictMode } from 'react'
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

            <ReactQueryDevtools />
          </UIProviders>
        </ErrorBoundary>
      </QueryClientProvider>
    </StrictMode>
  )
}
