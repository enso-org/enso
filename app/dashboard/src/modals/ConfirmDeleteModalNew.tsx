/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as z from 'zod'

import { Text } from '#/components/aria'
import { Button, ButtonGroup, Dialog, Form } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

// =============================
// === ConfirmDeleteModalNew ===
// =============================

/** Props for a {@link ConfirmDeleteModalNew}. */
export interface ConfirmDeleteModalNewProps {
  /** Must fit in the sentence "Are you sure you want to <action>?". */
  readonly actionText: string
  /** The label shown on the colored confirmation button. "Delete" by default. */
  readonly actionButtonLabel?: string
  readonly doDelete: () => Promise<void> | void
}

/** A modal for confirming the deletion of an asset. */
export default function ConfirmDeleteModalNew(props: ConfirmDeleteModalNewProps) {
  const { actionText, actionButtonLabel = 'Delete', doDelete } = props
  const { getText } = useText()

  return (
    <Dialog>
      {({ close }) => (
        <Form
          action="dialog"
          schema={z.object({})}
          data-testid="confirm-delete-modal"
          onSubmit={doDelete}
        >
          <Text className="relative">{getText('confirmPrompt', actionText)}</Text>
          <ButtonGroup className="relative">
            <Form.Submit size="medium" variant="delete">
              {actionButtonLabel}
            </Form.Submit>
            <Button size="medium" variant="cancel" autoFocus className="relative" onPress={close}>
              {getText('cancel')}
            </Button>
          </ButtonGroup>
        </Form>
      )}
    </Dialog>
  )
}
