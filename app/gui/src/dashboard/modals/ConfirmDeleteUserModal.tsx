/** @file Modal for confirming delete of any type of asset. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import { useText } from '$/providers/react'
import * as z from 'zod'

/** Props for a {@link ConfirmDeleteUserModal}. */
export interface ConfirmDeleteUserModalProps {
  readonly doDelete: () => Promise<void>
}

/** A modal for confirming the deletion of a user. */
export function ConfirmDeleteUserModal(props: ConfirmDeleteUserModalProps) {
  const { doDelete } = props
  const { getText } = useText()

  return (
    <Dialog title={getText('areYouSure')} role="alertdialog" className="items-center">
      <Form
        schema={z.object({})}
        method="dialog"
        data-testid="confirm-delete-modal"
        ref={(element) => {
          element?.focus()
        }}
        tabIndex={-1}
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={doDelete}
      >
        <Text className="text-balance text-center">
          {getText('confirmDeleteUserAccountWarning')}
        </Text>
        <Button.Group className="w-min self-center">
          <Form.Submit variant="delete">
            {getText('confirmDeleteUserAccountButtonLabel')}
          </Form.Submit>
        </Button.Group>
      </Form>
    </Dialog>
  )
}
