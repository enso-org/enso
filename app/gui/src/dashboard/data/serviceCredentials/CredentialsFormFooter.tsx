/** @file Submit and cancel buttons, and form error for the credentials form. */
import { ButtonGroup, DialogDismiss, Form } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link CredentialsFormFooter}. */
export interface CredentialsFormFooterProps {
  readonly isCreating: boolean
  readonly canCancel: boolean
  readonly canReset: boolean
}

/** Submit and cancel buttons for the credentials form. */
export function CredentialsFormFooter(props: CredentialsFormFooterProps) {
  const { isCreating, canCancel, canReset } = props

  const { getText } = useText()

  return (
    <>
      <Form.FormError />

      <ButtonGroup className="mt-2">
        <Form.Submit>{isCreating ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <DialogDismiss />}
        {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
      </ButtonGroup>
    </>
  )
}
