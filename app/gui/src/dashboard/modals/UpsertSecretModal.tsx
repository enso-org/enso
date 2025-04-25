/** @file A modal for creating and editing a secret. */
import { Dialog } from '#/components/AriaComponents'
import UpsertSecretMenu, { type UpsertSecretMenuProps } from '#/layouts/UpsertSecretMenu'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link UpsertSecretModal}. */
export interface UpsertSecretModalProps extends Omit<UpsertSecretMenuProps, 'doCancel'> {
  readonly defaultOpen?: boolean
  /** Defaults to `true`. */
  readonly canCancel?: boolean
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretModal(props: UpsertSecretModalProps) {
  const { defaultOpen, canCancel = true, secretId } = props
  const { getText } = useText()

  const isCreatingSecret = secretId == null

  return (
    <Dialog
      title={isCreatingSecret ? getText('newSecret') : getText('editSecret')}
      modalProps={defaultOpen == null ? {} : { defaultOpen }}
      isDismissable={false}
    >
      <UpsertSecretMenu {...props} doCancel={canCancel ? 'close' : null} />
    </Dialog>
  )
}
