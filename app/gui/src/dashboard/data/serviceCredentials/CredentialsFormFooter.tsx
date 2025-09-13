/** @file Submit and cancel buttons, and form error for the credentials form. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { useText } from '$/providers/react'

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
      <Button.Group className="mt-2">
        <Form.Submit>{isCreating ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>}
        {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
      </Button.Group>

      <Form.FormError />
    </>
  )
}
