/** @file A React hook to prevent navigation. */
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useSetModal } from '#/providers/ModalProvider'
import { isOnElectron } from 'enso-common/src/detect'
import { createElement, useEffect } from 'react'
import { ConfirmCloseModal } from './ConfirmCloseModal'
import { setShouldClose, shouldClose } from './utilities'

/** Options for {@link usePreventNavigation}. */
export interface PreventNavigationOptions {
  readonly isEnabled?: boolean
  readonly message: string
}

/** Prevent navigating away from a page. */
export function usePreventNavigation(options: PreventNavigationOptions) {
  const { isEnabled = true, message } = options
  const { setModal } = useSetModal()
  const messageRef = useSyncRef(message)

  useEffect(() => {
    if (isEnabled) {
      const onBeforeUnload = (event: BeforeUnloadEvent) => {
        if (!isOnElectron()) {
          // Browsers have their own `beforeunload` handling.
          event.preventDefault()
        } else if (!shouldClose) {
          event.preventDefault()
          setModal(createElement(ConfirmCloseModal, { message: messageRef.current }))
        } else {
          // Allow the window to close. Set `shouldClose` to false just in case something goes wrong.
          setShouldClose(false)
        }
      }
      window.addEventListener('beforeunload', onBeforeUnload)
      return () => {
        window.removeEventListener('beforeunload', onBeforeUnload)
      }
    }
  }, [isEnabled, messageRef, setModal])
}
