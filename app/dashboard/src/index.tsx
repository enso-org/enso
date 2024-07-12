/** @file Authentication module used by Enso IDE & Cloud.
 *
 * This module declares the main DOM structure for the authentication/dashboard app. */
import * as React from 'react'

import * as sentry from '@sentry/react'
import * as reactQuery from '@tanstack/react-query'
import * as reactDOM from 'react-dom/client'
import * as reactRouter from 'react-router-dom'
import invariant from 'tiny-invariant'

import * as detect from 'enso-common/src/detect'

import type * as app from '#/App'
import App from '#/App'

import LoadingScreen from '#/pages/authentication/LoadingScreen'

import * as devtools from '#/components/Devtools'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'

import HttpClient from '#/utilities/HttpClient'

export type { AccessToken } from '#/utilities/accessToken'

export type { GraphEditorRunner } from '#/layouts/Editor'

// =================
// === Constants ===
// =================

/** The `id` attribute of the root element that the app will be rendered into. */
const ROOT_ELEMENT_ID = 'enso-dashboard'
/** The fraction of non-erroring interactions that should be sampled by Sentry. */
const SENTRY_SAMPLE_RATE = 0.005

// ===========
// === run ===
// ===========

/** Entrypoint for the authentication/dashboard app.
 *
 * Running this function finds a `div` element with the ID `dashboard`, and renders the
 * authentication/dashboard UI using React. It also handles routing and other interactions (e.g.,
 * for redirecting the user to/from the login page). */
export // This export declaration must be broken up to satisfy the `require-jsdoc` rule.
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax
function run(props: Omit<app.AppProps, 'httpClient' | 'portalRoot'>) {
  const { vibrancy, supportsDeepLinks, queryClient } = props
  if (
    !detect.IS_DEV_MODE &&
    process.env.ENSO_CLOUD_SENTRY_DSN != null &&
    process.env.ENSO_CLOUD_API_URL != null
  ) {
    sentry.init({
      dsn: process.env.ENSO_CLOUD_SENTRY_DSN,
      environment: process.env.ENSO_CLOUD_ENVIRONMENT,
      integrations: [
        new sentry.BrowserTracing({
          routingInstrumentation: sentry.reactRouterV6Instrumentation(
            React.useEffect,
            reactRouter.useLocation,
            reactRouter.useNavigationType,
            reactRouter.createRoutesFromChildren,
            reactRouter.matchRoutes
          ),
        }),
        sentry.extraErrorDataIntegration({ captureErrorCause: true }),
        sentry.replayIntegration(),
        new sentry.BrowserProfilingIntegration(),
      ],
      profilesSampleRate: SENTRY_SAMPLE_RATE,
      tracesSampleRate: SENTRY_SAMPLE_RATE,
      tracePropagationTargets: [process.env.ENSO_CLOUD_API_URL.split('//')[1] ?? ''],
      replaysSessionSampleRate: SENTRY_SAMPLE_RATE,
      replaysOnErrorSampleRate: 1.0,
    })
  }

  if (vibrancy) {
    document.body.classList.add('vibrancy')
  }

  const root = document.getElementById(ROOT_ELEMENT_ID)
  const portalRoot = document.querySelector('#enso-portal-root')

  invariant(root, 'Root element not found')
  invariant(portalRoot, 'PortalRoot element not found')

  // `supportsDeepLinks` will be incorrect when accessing the installed Electron app's pages
  // via the browser.
  const actuallySupportsDeepLinks = detect.IS_DEV_MODE
    ? supportsDeepLinks
    : supportsDeepLinks && detect.isOnElectron()

  const httpClient = new HttpClient()

  React.startTransition(() => {
    reactDOM.createRoot(root).render(
      <React.StrictMode>
        <reactQuery.QueryClientProvider client={queryClient}>
          <errorBoundary.ErrorBoundary>
            <suspense.Suspense fallback={<LoadingScreen />}>
              <App
                {...props}
                supportsDeepLinks={actuallySupportsDeepLinks}
                portalRoot={portalRoot}
                httpClient={httpClient}
              />
            </suspense.Suspense>
          </errorBoundary.ErrorBoundary>

          <devtools.ReactQueryDevtools />
        </reactQuery.QueryClientProvider>
      </React.StrictMode>
    )
  })
}

/** Global configuration for the {@link App} component. */
export type AppProps = app.AppProps
