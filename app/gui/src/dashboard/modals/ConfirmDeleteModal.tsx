/** @file Modal for confirming delete of any type of asset. */
import { Alert, AlertDialog, Text, type Confirmable } from '#/components/AriaComponents'
import { useText } from '$/providers/react'

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps extends Confirmable {
  readonly defaultOpen?: boolean | undefined
  readonly cannotUndo?: boolean | undefined
  /** Must fit in the sentence "Are you sure you want to <action>?". */
  readonly actionText: string
  /** The label shown on the colored confirmation button. "Delete" by default. */
  readonly actionButtonLabel?: string | undefined
}

/** A modal for confirming the deletion of an asset. */
export default function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
  const {
    // MUST NOT be defaulted. Omitting this value should fall back to `Dialog`'s behavior.
    defaultOpen,
    cannotUndo = false,
    actionText,
    actionButtonLabel = 'Delete',
    onCancel,
    onConfirm,
  } = props

  const { getText } = useText()

  return (
    <AlertDialog
      title={getText('areYouSure')}
      modalProps={defaultOpen == null ? {} : { defaultOpen }}
      onConfirm={onConfirm}
      onCancel={onCancel}
      confirm={actionButtonLabel}
      isDestructive
    >
      <Text className="relative">{getText('confirmPrompt', actionText)}</Text>

      {cannotUndo && (
        <Alert variant="outline" icon="warning">
          {getText('thisOperationCannotBeUndone')}
        </Alert>
      )}
    </AlertDialog>
  )
}
