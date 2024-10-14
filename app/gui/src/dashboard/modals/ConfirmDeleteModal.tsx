/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import { ButtonGroup, Dialog, Form, Text } from '#/components/AriaComponents'
import { useSetModal } from '#/providers/ModalProvider'
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
  const { defaultOpen, actionText, actionButtonLabel = 'Delete', doDelete } = props

  const { unsetModal } = useSetModal()
  const { getText } = useText()

  return (
    <Dialog
      title={getText('areYouSure')}
      role="alertdialog"
      modalProps={defaultOpen == null ? {} : { defaultOpen }}
    >
      <Form schema={z.object({})} method="dialog" onSubmit={doDelete} onSubmitSuccess={unsetModal}>
        <Text className="relative">{getText('confirmPrompt', actionText)}</Text>

        <ButtonGroup>
          <Form.Submit variant="delete" className="relative">
            {actionButtonLabel}
          </Form.Submit>
          <Form.Submit action="cancel" />
        </ButtonGroup>
      </Form>
    </Dialog>
  )
}
