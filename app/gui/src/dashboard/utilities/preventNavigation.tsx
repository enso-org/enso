/** @file A React hook to prevent navigation. */
import { useEffect } from 'react'

import { Button, ButtonGroup, Dialog, Text } from '#/components/AriaComponents'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { isOnElectron } from 'enso-common/src/detect'

let shouldClose = false

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
          setModal(<ConfirmCloseModal message={messageRef.current} />)
        } else {
          // Allow the window to close. Set `shouldClose` to false just in case something goes wrong.
          shouldClose = false
        }
      }
      window.addEventListener('beforeunload', onBeforeUnload)
      return () => {
        window.removeEventListener('beforeunload', onBeforeUnload)
      }
    }
  }, [isEnabled, messageRef, setModal])
}

/** Props for a {@link ConfirmCloseModal}. */
interface ConfirmCloseModalProps {
  readonly message: string
}

/** A modal to confirm closing the window. */
function ConfirmCloseModal(props: ConfirmCloseModalProps) {
  const { message } = props
  const { getText } = useText()
  const { unsetModal } = useSetModal()

  return (
    <Dialog title={getText('closeWindowDialogTitle')} modalProps={{ defaultOpen: true }}>
      <Text>{message}</Text>
      <ButtonGroup>
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
      </ButtonGroup>
    </Dialog>
  )
}
