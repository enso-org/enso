/**
 * @file File containing the {@link App} React component, which is the entrypoint into our React
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
 * {@link authProvider.FullUserSession}).
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'
import * as z from 'zod'

import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import AuthProvider, * as authProvider from '#/providers/AuthProvider'
import BackendProvider, { useLocalBackend } from '#/providers/BackendProvider'
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

import * as openAppWatcher from '#/layouts/OpenAppWatcher'
import VersionChecker from '#/layouts/VersionChecker'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'
import { RouterProvider } from 'react-aria-components'

import AboutModal from '#/modals/AboutModal'
import { AgreementsModal } from '#/modals/AgreementsModal'
import { SetupOrganizationAfterSubscribe } from '#/modals/SetupOrganizationAfterSubscribe'

import LocalBackend from '#/services/LocalBackend'
import ProjectManager, * as projectManager from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'

import * as eventModule from '#/utilities/event'
import LocalStorage from '#/utilities/LocalStorage'
import { Path } from '#/utilities/path'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'

import { useInitAuthService } from '#/authentication/service'
import { useOffline } from '#/hooks/offlineHooks'
import { InvitedToOrganizationModal } from '#/modals/InvitedToOrganizationModal'
import { CloudBrowserDisabledLayout } from '#/providers/AuthProvider'
import { useHttpClient } from './providers/HttpClientProvider'
import { useMutationCallback } from './utilities/tanstackQuery'
import { unsafeWriteValue } from './utilities/write'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly localRootDirectory: string
    readonly preferredTimeZone: string
  }
}
LocalStorage.registerKey('localRootDirectory', { schema: z.string() })
LocalStorage.registerKey('preferredTimeZone', { schema: z.string() })

/** Returns the URL to the main page. This is the current URL, with the current route removed. */
function getMainPageUrl() {
  const mainPageUrl = new URL(window.location.href)
  mainPageUrl.pathname = mainPageUrl.pathname.replace(appUtils.ALL_PATHS_REGEX, '')
  return mainPageUrl
}

/** Global configuration for the `App` component. */
export interface AppProps {
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  /** If true, the app can only be used in offline mode. */
  readonly isAuthenticationDisabled: boolean
  /**
   * Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows.
   */
  readonly supportsDeepLinks: boolean
  /** The name of the project to open on startup, if any. */
  readonly initialProjectName: string | null
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly projectManagerUrl: string | null
  readonly ydocUrl: string | null
}

/**
 * Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app.
 */
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
    networkMode: 'always',
    ...STATIC_QUERY_OPTIONS,
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
        const response = await fetch(`/api/root-directory`)
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

  const { isOffline } = useOffline()
  const { getText } = textProvider.useText()
  const queryClient = reactQuery.useQueryClient()

  const executeBackgroundUpdate = useMutationCallback({
    mutationKey: ['refetch-queries', { isOffline }],
    scope: { id: 'refetch-queries' },
    mutationFn: () => queryClient.refetchQueries({ type: 'all', queryKey: [RemoteBackend.type] }),
    networkMode: 'online',
    onError: () => {
      toastify.toast.error(getText('refetchQueriesError'), {
        position: 'bottom-right',
      })
    },
  })

  React.useEffect(() => {
    if (!isOffline) {
      void executeBackgroundUpdate()
    }
  }, [executeBackgroundUpdate, isOffline])

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
        transition={toastify.Slide}
        limit={3}
      />
      <router.BrowserRouter
        basename={getMainPageUrl().pathname}
        // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
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

/** Props for an {@link AppRouter}. */
export interface AppRouterProps extends AppProps {
  readonly projectManagerRootDirectory: projectManager.Path | null
  readonly projectManagerInstance: ProjectManager | null
}

/**
 * Router definition for the app.
 *
 * The only reason the {@link AppRouter} component is separate from the {@link App} component is
 * because the {@link AppRouter} relies on React hooks, which can't be used in the same React
 * component as the component that defines the provider.
 */
function AppRouter(props: AppRouterProps) {
  const { onAuthenticated, projectManagerInstance } = props
  const httpClient = useHttpClient()
  const logger = useLogger()
  const navigate = router.useNavigate()

  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { setModal } = modalProvider.useSetModal()

  const navigator2D = navigator2DProvider.useNavigator2D()

  const localBackend =
    projectManagerInstance != null ? new LocalBackend(projectManagerInstance) : null

  const remoteBackend = new RemoteBackend(httpClient, logger, getText)

  if (detect.IS_DEV_MODE) {
    // @ts-expect-error This is used exclusively for debugging.
    unsafeWriteValue(window, 'navigate', navigate)
  }

  const mainPageUrl = getMainPageUrl()

  const authService = useInitAuthService(props)

  const registerAuthEventListener = authService.registerAuthEventListener

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
            <router.Route
              element={<CloudBrowserDisabledLayout redirectPath={appUtils.SETUP_PATH} />}
            >
              <router.Route element={<SetupOrganizationAfterSubscribe />}>
                <router.Route element={<InvitedToOrganizationModal />}>
                  <router.Route element={<openAppWatcher.OpenAppWatcher />}>
                    <router.Route
                      path={appUtils.DASHBOARD_PATH}
                      element={<Dashboard {...props} />}
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
        <router.Route element={<authProvider.AnyLoggedInUserLayout />}>
          <router.Route element={<authProvider.NotDeletedUserLayout />}>
            <router.Route
              element={<CloudBrowserDisabledLayout redirectPath={appUtils.SETUP_PATH} />}
            >
              <router.Route path={appUtils.SETUP_PATH} element={<setup.Setup />} />
            </router.Route>
          </router.Route>
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
    <RouterProvider navigate={navigate}>
      <SessionProvider
        onLogout={() => {
          localStorage.clearUserSpecificEntries()
        }}
        authService={authService.cognito}
        mainPageUrl={mainPageUrl}
        registerAuthEventListener={registerAuthEventListener}
      >
        <BackendProvider remoteBackend={remoteBackend} localBackend={localBackend}>
          <AuthProvider onAuthenticated={onAuthenticated}>
            <InputBindingsProvider>
              <LocalBackendPathSynchronizer />
              <VersionChecker />
              {routes}
            </InputBindingsProvider>
          </AuthProvider>
        </BackendProvider>
      </SessionProvider>
    </RouterProvider>
  )
}

/** Keep `localBackend.rootPath` in sync with the saved root path state. */
function LocalBackendPathSynchronizer() {
  const [localRootDirectory] = localStorageProvider.useLocalStorageState('localRootDirectory')
  const localBackend = useLocalBackend()
  if (localBackend) {
    if (localRootDirectory != null) {
      localBackend.setRootPath(Path(localRootDirectory))
    } else {
      localBackend.resetRootPath()
    }
  }

  return null
}
