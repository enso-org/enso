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
    <Dialog title={getText('areYouSure')}>
      <Form
        schema={z.object({})}
        method="dialog"
        data-testid="confirm-delete-modal"
        ref={(element) => {
          element?.focus()
        }}
        tabIndex={-1}
        className="pointer-events-auto relative flex w-confirm-delete-user-modal flex-col items-center gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={doDelete}
      >
        <Text className="relative mb-2 text-balance text-center">
          {getText('confirmDeleteUserAccountWarning')}
        </Text>
        <ButtonGroup className="relative self-center">
          <Form.Submit
            size="custom"
            variant="custom"
            className="button relative bg-danger text-inversed active"
          >
            <Text className="text">{getText('confirmDeleteUserAccountButtonLabel')}</Text>
          </Form.Submit>
        </ButtonGroup>
      </Form>
    </Dialog>
  )
}
