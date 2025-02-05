/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import { ButtonGroup, Dialog, Form, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
  readonly defaultOpen?: boolean | undefined
  readonly cannotUndo?: boolean | undefined
  /** Must fit in the sentence "Are you sure you want to <action>?". */
  readonly actionText: string
  /** The label shown on the colored confirmation button. "Delete" by default. */
  readonly actionButtonLabel?: string | undefined
  readonly doDelete: () => Promise<void>
}

/** A modal for confirming the deletion of an asset. */
export default function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
  const {
    // MUST NOT be defaulted. Omitting this value should fall back to `Dialog`'s behavior.
    defaultOpen,
    cannotUndo = false,
    actionText,
    actionButtonLabel = 'Delete',
    doDelete,
  } = props

  const { getText } = useText()

  return (
    <Dialog
      title={getText('areYouSure')}
      role="alertdialog"
      modalProps={defaultOpen == null ? {} : { defaultOpen }}
    >
      <Form schema={z.object({})} method="dialog" onSubmit={doDelete}>
        <Text className="relative">{getText('confirmPrompt', actionText)}</Text>
        {cannotUndo && (
          <Text className="relative" weight="bold">
            {getText('thisOperationCannotBeUndone')}
          </Text>
        )}

        <ButtonGroup>
          <Form.Submit autoFocus variant="delete" className="relative">
            {actionButtonLabel}
          </Form.Submit>

          <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
        </ButtonGroup>
      </Form>
    </Dialog>
  )
}
