/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import UnstyledButton from '#/components/styled/UnstyledButton'

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
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()

  const doSubmit = async () => {
    unsetModal()
    try {
      await doDelete()
    } catch (error) {
      toastAndLog(null, error)
    }
  }

  return (
    <Modal centered className="bg-dim">
      <form
        data-testid="confirm-delete-modal"
        ref={element => {
          element?.focus()
        }}
        tabIndex={-1}
        className="pointer-events-auto relative flex w-confirm-delete-user-modal flex-col items-center gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onKeyDown={event => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
          event.preventDefault()
          void doSubmit()
        }}
      >
        <h3 className="py-heading relative h-heading text-xl font-bold">Are you sure?</h3>
        <span className="relative">Once deleted, this user account will be gone forever.</span>
        <div>
          <ButtonRow>
            <UnstyledButton
              className="button relative bg-danger text-inversed active"
              onPress={doSubmit}
            >
              <aria.Text className="text">
                I confirm that I want to delete this user account.
              </aria.Text>
            </UnstyledButton>
          </ButtonRow>
        </div>
      </form>
    </Modal>
  )
}
