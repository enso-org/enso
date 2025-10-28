/**
 * @file File containing the {@link App} React component, which is the entrypoint into our React
 * application.
 *
 * The {@link App} component is responsible for defining the global context used by child
 * components. For example, it defines a {@link toastify.ToastContainer}, which is used to display temporary
 * notifications to the user. These global components are defined at the top of the {@link App} so
 * that they are available to all of the child components.
 *
 * The {@link App} also defines various providers.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toastify from 'react-toastify'
import * as z from 'zod'

import * as detect from 'enso-common/src/utilities/detect'

import InputBindingsProvider from '#/providers/InputBindingsProvider'
import ModalProvider from '#/providers/ModalProvider'

import VersionChecker from '#/layouts/VersionChecker'
import { RouterProvider } from 'react-aria-components'

import { AboutModal } from '#/modals/AboutModal'

import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'

import * as eventModule from '#/utilities/event'
import LocalStorage from '#/utilities/LocalStorage'

import { useOffline } from '#/hooks/offlineHooks'
import type { ModalApi } from '#/utilities/modal'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { unsafeWriteValue } from '#/utilities/write'
import { useRouter, useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly preferredTimeZone: string
    readonly loginRedirect: string
  }
}
LocalStorage.registerKey('preferredTimeZone', { schema: z.string() })
LocalStorage.registerKey('loginRedirect', {
  isUserSpecific: true,
  schema: z.string(),
})

window.api?.menu.setMenuItemHandler('about', () => {
  AboutModal.open()
})

/**
 * Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app.
 */
export default function App(props: React.PropsWithChildren) {
  const { isOffline } = useOffline()
  const { getText } = useText()
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

  // `InputBindingsProvider` depends on `LocalStorageProvider`.
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
      <ModalProvider>
        <AppRouter {...props} />
      </ModalProvider>
    </>
  )
}

/**
 * Router definition for the app.
 *
 * The only reason the {@link AppRouter} component is separate from the {@link App} component is
 * because the {@link AppRouter} relies on React hooks, which can't be used in the same React
 * component as the component that defines the provider.
 */
function AppRouter(props: React.PropsWithChildren) {
  const { children } = props
  const { router } = useRouter()
  const navigate = router.push.bind(router)

  if (detect.IS_DEV_MODE) {
    // @ts-expect-error This is used exclusively for debugging.
    unsafeWriteValue(window, 'navigate', navigate)
  }

  const aboutModalRef = React.useRef<ModalApi>(null)

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
        const app = document.getElementById('ProjectView')
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

  return (
    <RouterProvider navigate={navigate}>
      <InputBindingsProvider>
        <VersionChecker />
        <ThemeSynchronizer />
        <AboutModal ref={aboutModalRef} />
        {children}
      </InputBindingsProvider>
    </RouterProvider>
  )
}

/** Keep theme class on document body in sync with saved theme state. */
function ThemeSynchronizer() {
  const isDarkTheme = useFeatureFlag('unsafeDarkTheme')

  React.useEffect(() => {
    if (isDarkTheme) {
      document.documentElement.classList.add('theme-dark')
    } else {
      document.documentElement.classList.remove('theme-dark')
    }
    localStorage.setItem('enso-theme', isDarkTheme ? 'dark' : 'light')
  }, [isDarkTheme])

  return null
}
