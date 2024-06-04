/**
 * @file
 *
 * This file is the main layout for the application. It is responsible for setting up the providers and hooks that are used throughout the application.
 */
import * as React from 'react'

import * as router from 'react-router-dom'

import * as detect from 'enso-common/src/detect'

import * as inputBindingsModule from '#/configurations/inputBindings'

import * as backendHooks from '#/hooks/backendHooks'

import * as appProvider from '#/providers/AppProvider'
import AuthProvider from '#/providers/AuthProvider'
import BackendProvider from '#/providers/BackendProvider'
import * as httpClientProvider from '#/providers/HttpClientProvider'
import InputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import LoggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import SessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as offlineNotificationManager from '#/components/OfflineNotificationManager'
import * as paywall from '#/components/Paywall'
import * as rootComponent from '#/components/Root'

import AboutModal from '#/modals/AboutModal'

import LocalBackend from '#/services/LocalBackend'
import type * as projectManager from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'

import * as eventModule from '#/utilities/event'
import * as object from '#/utilities/object'

import * as authServiceModule from '#/authentication/service'

import type * as appComponent from './App'

/**
 *
 */
/** Props for an {@link AppLayout}. */
export interface AppLayoutProps extends appComponent.AppProps {
  readonly projectManagerRootDirectory: projectManager.Path | null
  readonly mainPageUrl: URL
}

/**
 * Root Layout of the App. Provides core data and dependencies to
 * the nested elements
 */
export function AppLayout(props: AppLayoutProps) {
  const { logger, isAuthenticationDisabled, httpClient, mainPageUrl } = props
  const { onAuthenticated, projectManagerUrl, projectManagerRootDirectory } = props
  const { portalRoot } = props
  // `navigateHooks.useNavigate` cannot be used here as it relies on `AuthProvider`, which has not
  // yet been initialized at this point.
  // eslint-disable-next-line no-restricted-properties
  const navigate = router.useNavigate()
  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { setModal } = modalProvider.useSetModal()
  const navigator2D = navigator2DProvider.useNavigator2D()

  const localBackend = React.useMemo(
    () =>
      projectManagerUrl != null && projectManagerRootDirectory != null
        ? new LocalBackend(projectManagerUrl, projectManagerRootDirectory)
        : null,
    [projectManagerUrl, projectManagerRootDirectory]
  )

  const remoteBackend = React.useMemo(
    () => new RemoteBackend(httpClient, logger, getText),
    [httpClient, logger, getText]
  )

  backendHooks.useObserveBackend(remoteBackend)
  backendHooks.useObserveBackend(localBackend)

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
        k => savedInputBindings[k]
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
          Object.entries(inputBindingsRaw.metadata).map(kv => {
            const [k, v] = kv
            return [k, v.bindings]
          })
        )
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

  const authService = React.useMemo(() => {
    const authConfig = { navigate, ...props }
    return authServiceModule.initAuthService(authConfig)
  }, [props, navigate])

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

  let result: React.JSX.Element | null = <router.Outlet />

  if (detect.IS_DEV_MODE) {
    result = <paywall.PaywallDevtools>{result}</paywall.PaywallDevtools>
  }

  result = <errorBoundary.ErrorBoundary>{result}</errorBoundary.ErrorBoundary>
  result = <InputBindingsProvider inputBindings={inputBindings}>{result}</InputBindingsProvider>
  result = (
    <AuthProvider
      shouldStartInOfflineMode={isAuthenticationDisabled}
      authService={authService}
      onAuthenticated={onAuthenticated}
    >
      {result}
    </AuthProvider>
  )

  result = (
    <BackendProvider remoteBackend={remoteBackend} localBackend={localBackend}>
      {result}
    </BackendProvider>
  )

  result = (
    <SessionProvider
      saveAccessToken={authService?.cognito.saveAccessToken.bind(authService.cognito) ?? null}
      mainPageUrl={mainPageUrl}
      userSession={userSession}
      registerAuthEventListener={registerAuthEventListener}
      refreshUserSession={refreshUserSession}
    >
      {result}
    </SessionProvider>
  )
  result = <LoggerProvider logger={logger}>{result}</LoggerProvider>
  result = (
    <rootComponent.Root navigate={navigate} portalRoot={portalRoot}>
      {result}
    </rootComponent.Root>
  )
  result = (
    <offlineNotificationManager.OfflineNotificationManager>
      {result}
    </offlineNotificationManager.OfflineNotificationManager>
  )
  result = (
    <httpClientProvider.HttpClientProvider httpClient={httpClient}>
      {result}
    </httpClientProvider.HttpClientProvider>
  )

  result = <LoggerProvider logger={logger}>{result}</LoggerProvider>
  result = <appProvider.AppProvider {...props}>{result}</appProvider.AppProvider>

  return result
}
