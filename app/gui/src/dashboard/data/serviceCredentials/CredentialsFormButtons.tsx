/** @file Submit and cancel buttons for the credentials form. */

import { ButtonGroup, DialogDismiss, Form } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link CredentialsFormButtons}. */
export interface CredentialsFormButtonsProps {
  readonly isCreating: boolean
  readonly canCancel: boolean
  readonly canReset: boolean
}

/** Submit and cancel buttons for the credentials form. */
export function CredentialsFormButtons(props: CredentialsFormButtonsProps) {
  const { isCreating, canCancel, canReset } = props

  const { getText } = useText()

  return (
    <ButtonGroup className="mt-2">
      <Form.Submit>{isCreating ? getText('create') : getText('update')}</Form.Submit>
      {canCancel && <DialogDismiss />}
      {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
    </ButtonGroup>
  )
}
