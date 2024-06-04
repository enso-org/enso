/** @file File containing the {@link App} React component, which is the entrypoint into our React
 * application.
 *
 * # Providers
 *
 * The {@link App} component is responsible for defining the global context used by child
 * components. For example, it defines a {@link toastify.ToastContainer}, which is used to display temporary
 * notifications to the user. These global components are defined at the top of the {@link App} so
 * that they are available to all of the child components.
 *
 * The {@link App} also defines various providers (e.g., {@link authProvider.AuthProvider}).
 * Providers are a React-specific concept that allows components to access global state without
 * having to pass it down through the component tree. For example, the
 * {@link authProvider.AuthProvider} wraps the entire application, and provides the context
 * necessary for child components to use the {@link authProvider.useAuth} hook. The
 * {@link authProvider.useAuth} hook lets child components access the user's authentication session
 * (i.e., email, username, etc.) and it also provides methods for signing the user in, etc.
 *
 * Providers consist of a provider component that wraps the application, a context object defined
 * by the provider component, and a hook that can be used by child components to access the context.
 * All of the providers are initialized here, at the {@link App} component to ensure that they are
 * available to all of the child components.
 *
 * # Routes and Authentication
 *
 * The {@link AppRouter} component defines the layout of the application, in terms of navigation. It
 * consists of a list of {@link router.Route}s, as well as the HTTP pathnames that the
 * {@link router.Route}s can be accessed by.
 *
 * The {@link router.Route}s are grouped by authorization level. Some routes are
 * accessed by unauthenticated (i.e., not signed in) users. Some routes are accessed by partially
 * authenticated users (c.f. {@link authProvider.PartialUserSession}). That is, users who have
 * signed up but who have not completed email verification or set a username. The remaining
 * {@link router.Route}s require fully authenticated users (c.f.
 * {@link authProvider.FullUserSession}). */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'
import * as z from 'zod'

import * as appUtils from '#/appUtils'

import LocalStorageProvider from '#/providers/LocalStorageProvider'
import type * as loggerProvider from '#/providers/LoggerProvider'
import ModalProvider from '#/providers/ModalProvider'

import * as projectManager from '#/services/ProjectManager'

import * as appBaseUrl from '#/utilities/appBaseUrl'
import type HttpClient from '#/utilities/HttpClient'
import LocalStorage from '#/utilities/LocalStorage'

import type * as types from '../../types/types'
import * as appRoutes from './Routes'

// ============================
// === Global configuration ===
// ============================
const INPUT_BINDINGS_SCHEMA = z.record(z.array(z.string()).readonly()).or(z.null())

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly inputBindings: z.infer<typeof INPUT_BINDINGS_SCHEMA>
  }
}

LocalStorage.registerKey('inputBindings', { isUserSpecific: true, schema: INPUT_BINDINGS_SCHEMA })

// ======================
// === getMainPageUrl ===
// ======================

/** Returns the URL to the main page. This is the current URL, with the current route removed. */
function getMainPageUrl() {
  const mainPageUrl = new URL(window.location.href)
  mainPageUrl.pathname = mainPageUrl.pathname.replace(appUtils.ALL_PATHS_REGEX, '')
  return mainPageUrl
}

// ===========
// === App ===
// ===========

/** Global configuration for the `App` component. */
export interface AppProps {
  readonly vibrancy: boolean
  readonly logger: loggerProvider.Logger
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  /** If true, the app can only be used in offline mode. */
  readonly isAuthenticationDisabled: boolean
  /** Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows. */
  readonly supportsDeepLinks: boolean
  /** The name of the project to open on startup, if any. */
  readonly initialProjectName: string | null
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly projectManagerUrl: string | null
  readonly ydocUrl: string | null
  readonly appRunner: types.EditorRunner | null
  readonly portalRoot: Element
  readonly httpClient: HttpClient
}

/** Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app. */
export default function App(props: AppProps) {
  const { supportsLocalBackend } = props

  const { data: rootDirectoryPath } = reactQuery.useSuspenseQuery({
    queryKey: ['root-directory', supportsLocalBackend],
    meta: { persist: false },
    networkMode: 'always',
    queryFn: async () => {
      if (supportsLocalBackend) {
        const response = await fetch(`${appBaseUrl.APP_BASE_URL}/api/root-directory`)
        const text = await response.text()
        return projectManager.Path(text)
      } else {
        return null
      }
    },
  })

  const mainPageUrl = getMainPageUrl()

  // Both `BackendProvider` and `InputBindingsProvider` depend on `LocalStorageProvider`.
  // Note that the `Router` must be the parent of the `AuthProvider`, because the `AuthProvider`
  // will redirect the user between the login/register pages and the dashboard.
  return (
    <>
      <toastify.ToastContainer
        position="top-center"
        theme="light"
        closeOnClick={false}
        draggable={false}
        toastClassName="text-sm leading-cozy bg-selected-frame rounded-lg backdrop-blur-default"
        transition={toastify.Zoom}
        limit={3}
      />
      <LocalStorageProvider>
        <ModalProvider>
          <appRoutes.Routes
            {...props}
            basename={mainPageUrl.pathname}
            projectManagerRootDirectory={rootDirectoryPath}
            mainPageUrl={mainPageUrl}
          />
        </ModalProvider>
      </LocalStorageProvider>
    </>
  )
}
