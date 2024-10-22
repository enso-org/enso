/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import { ButtonGroup, Dialog, Form, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

// ==============================
// === ConfirmDeleteUserModal ===
// ==============================

/** Props for a {@link ConfirmDeleteUserModal}. */
export interface ConfirmDeleteUserModalProps {
  readonly doDelete: () => Promise<void>
}

/** A modal for confirming the deletion of a user. */
export default function ConfirmDeleteUserModal(props: ConfirmDeleteUserModalProps) {
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
        <ButtonGroup className="w-min self-center">
          <Form.Submit variant="delete">
            {getText('confirmDeleteUserAccountButtonLabel')}
          </Form.Submit>
        </ButtonGroup>
      </Form>
    </Dialog>
  )
}
