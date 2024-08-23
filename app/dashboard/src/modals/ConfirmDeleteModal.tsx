/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import { Button, ButtonGroup, Dialog, Form, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

// ==========================
// === ConfirmDeleteModal ===
// ==========================

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
  readonly defaultOpen?: boolean
  /** Must fit in the sentence "Are you sure you want to <action>?". */
  readonly actionText: string
  /** The label shown on the colored confirmation button. "Delete" by default. */
  readonly actionButtonLabel?: string
  readonly doDelete: () => void
}

/** A modal for confirming the deletion of an asset. */
export default function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
  const { defaultOpen = false, actionText, actionButtonLabel = 'Delete', doDelete } = props
  const { getText } = useText()

  return (
    <Dialog title={getText('areYouSure')} modalProps={{ defaultOpen }}>
      {({ close }) => (
        <Form
          schema={z.object({})}
          method="dialog"
          data-testid="confirm-delete-modal"
          tabIndex={-1}
          onSubmit={doDelete}
        >
          <Text className="relative">{getText('confirmPrompt', actionText)}</Text>
          <ButtonGroup className="relative">
            <Form.Submit variant="delete" className="relative">
              {actionButtonLabel}
            </Form.Submit>
            <Button size="medium" variant="cancel" autoFocus className="relative" onPress={close}>
              {getText('cancel')}
            </Button>
          </ButtonGroup>
        </Form>
      )}
    </Dialog>
  )
}
