/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

// ==============================
// === ConfirmDeleteUserModal ===
// ==============================

/** Props for a {@link ConfirmDeleteUserModal}. */
export interface ConfirmDeleteUserModalProps {
  readonly description: string
  readonly doDelete: () => Promise<void>
}

/** A modal for confirming the deletion of a user. */
export default function ConfirmDeleteUserModal(props: ConfirmDeleteUserModalProps) {
  const { description, doDelete } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()

  const onSubmit = async () => {
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
        className="relative flex flex-col items-center gap-2 rounded-default w-96 p-2 pointer-events-auto before:absolute before:inset before:rounded-default before:bg-selected-frame before:backdrop-blur-default before:w-full before:h-full"
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
          void onSubmit()
        }}
      >
        <h3 className="relative font-bold text-xl h-9.5 py-0.5">Are you sure?</h3>
        <div className="relative flex flex-col gap-2">
          Once deleted, this {description} will be gone forever.
          <button type="submit" className="rounded-full bg-danger text-inversed px-2 py-1">
            <span className="text">I confirm that I want to delete this {description}.</span>
          </button>
        </div>
      </form>
    </Modal>
  )
}
