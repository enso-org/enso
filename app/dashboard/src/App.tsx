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

import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import * as inputBindingsModule from '#/configurations/inputBindings'

import AuthProvider, * as authProvider from '#/providers/AuthProvider'
import BackendProvider from '#/providers/BackendProvider'
import DriveProvider from '#/providers/DriveProvider'
import DevtoolsProvider from '#/providers/EnsoDevtoolsProvider'
import { useHttpClient } from '#/providers/HttpClientProvider'
import InputBindingsProvider from '#/providers/InputBindingsProvider'
import LocalStorageProvider, * as localStorageProvider from '#/providers/LocalStorageProvider'
import { useLogger } from '#/providers/LoggerProvider'
import ModalProvider, * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import SessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import Registration from '#/pages/authentication/Registration'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import * as setup from '#/pages/authentication/Setup'
import Dashboard from '#/pages/dashboard/Dashboard'
import * as subscribe from '#/pages/subscribe/Subscribe'
import * as subscribeSuccess from '#/pages/subscribe/SubscribeSuccess'

import type * as editor from '#/layouts/Editor'
import * as openAppWatcher from '#/layouts/OpenAppWatcher'
import VersionChecker from '#/layouts/VersionChecker'

import { RouterProvider } from '#/components/aria'
import * as devtools from '#/components/Devtools'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'

import AboutModal from '#/modals/AboutModal'
import { AgreementsModal } from '#/modals/AgreementsModal'
import * as setOrganizationNameModal from '#/modals/SetOrganizationNameModal'

import LocalBackend from '#/services/LocalBackend'
import ProjectManager, * as projectManager from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'

import * as appBaseUrl from '#/utilities/appBaseUrl'
import * as eventModule from '#/utilities/event'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'

import { useInitAuthService } from '#/authentication/service'
import { InvitedToOrganizationModal } from '#/modals/InvitedToOrganizationModal'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly inputBindings: Readonly<Record<string, readonly string[]>>
  }
}

LocalStorage.registerKey('inputBindings', {
  schema: z.record(z.string().array().readonly()).transform((value) =>
    Object.fromEntries(
      Object.entries<unknown>({ ...value }).flatMap((kv) => {
        const [k, v] = kv
        return Array.isArray(v) && v.every((item): item is string => typeof item === 'string') ?
            [[k, v]]
          : []
      }),
    ),
  ),
})

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
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  /** If true, the app can only be used in offline mode. */
  readonly isAuthenticationDisabled: boolean
  /** Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows. */
  readonly supportsDeepLinks: boolean
  /** Whether the dashboard should be rendered. */
  readonly shouldShowDashboard: boolean
  /** The name of the project to open on startup, if any. */
  readonly initialProjectName: string | null
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly projectManagerUrl: string | null
  readonly ydocUrl: string | null
  readonly appRunner: editor.GraphEditorRunner | null
  readonly queryClient: reactQuery.QueryClient
}

/** Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app. */
export default function App(props: AppProps) {
  const {
    data: { projectManagerRootDirectory, projectManagerInstance },
  } = reactQuery.useSuspenseQuery<{
    projectManagerInstance: ProjectManager | null
    projectManagerRootDirectory: projectManager.Path | null
  }>({
    queryKey: [
      'root-directory',
      {
        projectManagerUrl: props.projectManagerUrl,
        supportsLocalBackend: props.supportsLocalBackend,
      },
    ] as const,
    meta: { persist: false },
    networkMode: 'always',
    staleTime: Infinity,
    gcTime: Infinity,
    refetchOnMount: false,
    refetchInterval: false,
    refetchOnReconnect: false,
    refetchIntervalInBackground: false,
    behavior: {
      onFetch: ({ state }) => {
        const instance = state.data?.projectManagerInstance ?? null

        if (instance != null) {
          void instance.dispose()
        }
      },
    },
    queryFn: async () => {
      if (props.supportsLocalBackend && props.projectManagerUrl != null) {
        const response = await fetch(`${appBaseUrl.APP_BASE_URL}/api/root-directory`)
        const text = await response.text()
        const rootDirectory = projectManager.Path(text)

        return {
          projectManagerInstance: new ProjectManager(props.projectManagerUrl, rootDirectory),
          projectManagerRootDirectory: rootDirectory,
        }
      } else {
        return {
          projectManagerInstance: null,
          projectManagerRootDirectory: null,
        }
      }
    },
  })

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
      <router.BrowserRouter basename={getMainPageUrl().pathname}>
        <LocalStorageProvider>
          <ModalProvider>
            <AppRouter
              {...props}
              projectManagerInstance={projectManagerInstance}
              projectManagerRootDirectory={projectManagerRootDirectory}
            />
          </ModalProvider>
        </LocalStorageProvider>
      </router.BrowserRouter>
    </>
  )
}

// =================
// === AppRouter ===
// =================

/** Props for an {@link AppRouter}. */
export interface AppRouterProps extends AppProps {
  readonly projectManagerRootDirectory: projectManager.Path | null
  readonly projectManagerInstance: ProjectManager | null
}

/** Router definition for the app.
 *
 * The only reason the {@link AppRouter} component is separate from the {@link App} component is
 * because the {@link AppRouter} relies on React hooks, which can't be used in the same React
 * component as the component that defines the provider. */
function AppRouter(props: AppRouterProps) {
  const { isAuthenticationDisabled, shouldShowDashboard } = props
  const { onAuthenticated, projectManagerInstance } = props
  const httpClient = useHttpClient()
  const logger = useLogger()
  const navigate = router.useNavigate()
  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { setModal } = modalProvider.useSetModal()
  const navigator2D = navigator2DProvider.useNavigator2D()

  const localBackend = React.useMemo(
    () => (projectManagerInstance != null ? new LocalBackend(projectManagerInstance) : null),
    [projectManagerInstance],
  )

  const remoteBackend = React.useMemo(
    () => new RemoteBackend(httpClient, logger, getText),
    [httpClient, logger, getText],
  )

  if (detect.IS_DEV_MODE) {
    // @ts-expect-error This is used exclusively for debugging.
    window.navigate = navigate
  }

  const [inputBindingsRaw] = React.useState(() => inputBindingsModule.createBindings())

  React.useEffect(() => {
    const savedInputBindings = localStorage.get('inputBindings')
    if (savedInputBindings != null) {
      const filteredInputBindings = object.mapEntries(
        inputBindingsRaw.metadata,
        (k) => savedInputBindings[k],
      )
      for (const [bindingKey, newBindings] of object.unsafeEntries(filteredInputBindings)) {
        for (const oldBinding of inputBindingsRaw.metadata[bindingKey].bindings) {
          inputBindingsRaw.delete(bindingKey, oldBinding)
        }
        for (const newBinding of newBindings ?? []) {
          inputBindingsRaw.add(bindingKey, newBinding)
        }
      }
    }
  }, [localStorage, inputBindingsRaw])

  const inputBindings = React.useMemo(() => {
    const updateLocalStorage = () => {
      localStorage.set(
        'inputBindings',
        Object.fromEntries(
          Object.entries(inputBindingsRaw.metadata).map((kv) => {
            const [k, v] = kv
            return [k, v.bindings]
          }),
        ),
      )
    }
    return {
      /** Transparently pass through `handler()`. */
      get handler() {
        return inputBindingsRaw.handler.bind(inputBindingsRaw)
      },
      /** Transparently pass through `attach()`. */
      get attach() {
        return inputBindingsRaw.attach.bind(inputBindingsRaw)
      },
      reset: (bindingKey: inputBindingsModule.DashboardBindingKey) => {
        inputBindingsRaw.reset(bindingKey)
        updateLocalStorage()
      },
      add: (bindingKey: inputBindingsModule.DashboardBindingKey, binding: string) => {
        inputBindingsRaw.add(bindingKey, binding)
        updateLocalStorage()
      },
      delete: (bindingKey: inputBindingsModule.DashboardBindingKey, binding: string) => {
        inputBindingsRaw.delete(bindingKey, binding)
        updateLocalStorage()
      },
      /** Transparently pass through `metadata`. */
      get metadata() {
        return inputBindingsRaw.metadata
      },
      /** Transparently pass through `register()`. */
      get register() {
        return inputBindingsRaw.unregister.bind(inputBindingsRaw)
      },
      /** Transparently pass through `unregister()`. */
      get unregister() {
        return inputBindingsRaw.unregister.bind(inputBindingsRaw)
      },
    }
  }, [localStorage, inputBindingsRaw])
  const mainPageUrl = getMainPageUrl()
  const authService = useInitAuthService(props)
  const userSession = authService?.cognito.userSession.bind(authService.cognito) ?? null
  const refreshUserSession =
    authService?.cognito.refreshUserSession.bind(authService.cognito) ?? null
  const registerAuthEventListener = authService?.registerAuthEventListener ?? null

  React.useEffect(() => {
    if ('menuApi' in window) {
      window.menuApi.setShowAboutModalHandler(() => {
        setModal(<AboutModal />)
      })
    }
  }, [setModal])

  React.useEffect(() => {
    const onKeyDown = navigator2D.onKeyDown.bind(navigator2D)
    document.addEventListener('keydown', onKeyDown)
    return () => {
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [navigator2D])

  React.useEffect(() => {
    let isClick = false
    const onMouseDown = () => {
      isClick = true
    }
    const onMouseUp = (event: MouseEvent) => {
      if (
        isClick &&
        !eventModule.isElementTextInput(event.target) &&
        !eventModule.isElementPartOfMonaco(event.target) &&
        !eventModule.isElementTextInput(document.activeElement)
      ) {
        const selection = document.getSelection()
        const app = document.getElementById('app')
        const appContainsSelection =
          app != null &&
          selection != null &&
          selection.anchorNode != null &&
          app.contains(selection.anchorNode) &&
          selection.focusNode != null &&
          app.contains(selection.focusNode)
        if (!appContainsSelection) {
          selection?.removeAllRanges()
        }
      }
    }
    const onSelectStart = () => {
      isClick = false
    }

    document.addEventListener('mousedown', onMouseDown)
    document.addEventListener('mouseup', onMouseUp)
    document.addEventListener('selectstart', onSelectStart)
    return () => {
      document.removeEventListener('mousedown', onMouseDown)
      document.removeEventListener('mouseup', onMouseUp)
      document.removeEventListener('selectstart', onSelectStart)
    }
  }, [])

  const routes = (
    <router.Routes>
      {/* Login & registration pages are visible to unauthenticated users. */}
      <router.Route element={<authProvider.GuestLayout />}>
        <router.Route path={appUtils.REGISTRATION_PATH} element={<Registration />} />
        <router.Route path={appUtils.LOGIN_PATH} element={<Login />} />
      </router.Route>

      {/* Protected pages are visible to authenticated users. */}
      <router.Route element={<authProvider.NotDeletedUserLayout />}>
        <router.Route element={<authProvider.ProtectedLayout />}>
          <router.Route element={<AgreementsModal />}>
            <router.Route element={<setOrganizationNameModal.SetOrganizationNameModal />}>
              <router.Route element={<InvitedToOrganizationModal />}>
                <router.Route element={<openAppWatcher.OpenAppWatcher />}>
                  <router.Route
                    path={appUtils.DASHBOARD_PATH}
                    element={shouldShowDashboard && <Dashboard {...props} />}
                  />

                  <router.Route
                    path={appUtils.SUBSCRIBE_PATH}
                    element={
                      <errorBoundary.ErrorBoundary>
                        <suspense.Suspense>
                          <subscribe.Subscribe />
                        </suspense.Suspense>
                      </errorBoundary.ErrorBoundary>
                    }
                  />
                </router.Route>
              </router.Route>
            </router.Route>
          </router.Route>

          <router.Route
            path={appUtils.SUBSCRIBE_SUCCESS_PATH}
            element={
              <errorBoundary.ErrorBoundary>
                <suspense.Suspense>
                  <subscribeSuccess.SubscribeSuccess />
                </suspense.Suspense>
              </errorBoundary.ErrorBoundary>
            }
          />
        </router.Route>
      </router.Route>

      <router.Route element={<AgreementsModal />}>
        <router.Route element={<authProvider.NotDeletedUserLayout />}>
          <router.Route path={appUtils.SETUP_PATH} element={<setup.Setup />} />
        </router.Route>
      </router.Route>

      {/* Other pages are visible to unauthenticated and authenticated users. */}
      <router.Route path={appUtils.CONFIRM_REGISTRATION_PATH} element={<ConfirmRegistration />} />
      <router.Route path={appUtils.FORGOT_PASSWORD_PATH} element={<ForgotPassword />} />
      <router.Route path={appUtils.RESET_PASSWORD_PATH} element={<ResetPassword />} />

      {/* Soft-deleted user pages are visible to users who have been soft-deleted. */}
      <router.Route element={<authProvider.ProtectedLayout />}>
        <router.Route element={<authProvider.SoftDeletedUserLayout />}>
          <router.Route path={appUtils.RESTORE_USER_PATH} element={<RestoreAccount />} />
        </router.Route>
      </router.Route>

      {/* 404 page */}
      <router.Route path="*" element={<router.Navigate to="/" replace />} />
    </router.Routes>
  )

  return (
    <DevtoolsProvider>
      <RouterProvider navigate={navigate}>
        <SessionProvider
          saveAccessToken={authService?.cognito.saveAccessToken.bind(authService.cognito) ?? null}
          mainPageUrl={mainPageUrl}
          userSession={userSession}
          registerAuthEventListener={registerAuthEventListener}
          refreshUserSession={refreshUserSession}
        >
          <BackendProvider remoteBackend={remoteBackend} localBackend={localBackend}>
            <AuthProvider
              shouldStartInOfflineMode={isAuthenticationDisabled}
              authService={authService}
              onAuthenticated={onAuthenticated}
            >
              <InputBindingsProvider inputBindings={inputBindings}>
                {/* Ideally this would be in `Drive.tsx`, but it currently must be all the way out here
                 * due to modals being in `TheModal`. */}
                <DriveProvider>
                  <errorBoundary.ErrorBoundary>
                    <VersionChecker />
                    {routes}
                    {detect.IS_DEV_MODE && (
                      <suspense.Suspense>
                        <devtools.EnsoDevtools />
                      </suspense.Suspense>
                    )}
                  </errorBoundary.ErrorBoundary>
                </DriveProvider>
              </InputBindingsProvider>
            </AuthProvider>
          </BackendProvider>
        </SessionProvider>
      </RouterProvider>
    </DevtoolsProvider>
  )
}
