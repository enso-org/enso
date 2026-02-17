/** @file A React hook to prevent navigation. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Text } from '#/components/Text'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { useText } from '$/providers/react'
import { isOnElectron } from 'enso-common/src/utilities/detect'
import { useEffect } from 'react'

let shouldClose = false

/** Options for {@link usePreventNavigation}. */
interface PreventNavigationOptions {
  readonly isEnabled?: boolean
  readonly message: string
}

/** Prevent navigating away from a page. */
// eslint-disable-next-line react-refresh/only-export-components
export function usePreventNavigation(options: PreventNavigationOptions) {
  const { isEnabled = true, message } = options
  const messageRef = useSyncRef(message)

  useEffect(() => {
    if (isEnabled) {
      const onBeforeUnload = (event: BeforeUnloadEvent) => {
        if (!isOnElectron()) {
          // Browsers have their own `beforeunload` handling.
          event.preventDefault()
        } else if (!shouldClose) {
          event.preventDefault()
          setModal(<ConfirmCloseModal message={messageRef.current} />)
        } else {
          // Allow the window to close. Set `shouldClose` to false just in case something goes wrong.
          shouldClose = false
        }
        event.stopImmediatePropagation()
      }
      window.addEventListener('beforeunload', onBeforeUnload)
      return () => {
        window.removeEventListener('beforeunload', onBeforeUnload)
      }
    }
  }, [isEnabled, messageRef])
}

/** Props for a {@link ConfirmCloseModal}. */
interface ConfirmCloseModalProps {
  readonly message: string
}

/** A modal to confirm closing the window. */
export function ConfirmCloseModal(props: ConfirmCloseModalProps) {
  const { message } = props
  const { getText } = useText()

  return (
    <Dialog title={getText('closeWindowDialogTitle')} modalProps={{ defaultOpen: true }}>
      <Text>{message}</Text>
      <Button.Group>
        <Button
          variant="delete"
          onPress={() => {
            shouldClose = true
            window.close()
          }}
        >
          {getText('close')}
        </Button>
        <Button variant="outline" onPress={unsetModal}>
          {getText('cancel')}
        </Button>
      </Button.Group>
    </Dialog>
  )
}
