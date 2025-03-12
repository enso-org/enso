/** @file A React hook to prevent navigation. */
import { Button, ButtonGroup, Dialog, Text } from '#/components/AriaComponents'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { setShouldClose } from './hooks'

/** Props for a {@link ConfirmCloseModal}. */
interface ConfirmCloseModalProps {
  readonly message: string
}

/** A modal to confirm closing the window. */
export function ConfirmCloseModal(props: ConfirmCloseModalProps) {
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
            setShouldClose(true)
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
