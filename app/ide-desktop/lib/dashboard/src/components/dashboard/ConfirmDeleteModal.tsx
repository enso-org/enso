/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

// ==========================
// === ConfirmDeleteModal ===
// ==========================

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
  /** Must fit in the sentence "Are you sure you want to delete <description>?". */
  description: string
  doDelete: () => void
}

/** A modal for confirming the deletion of an asset. */
export default function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
  const { description, doDelete } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()

  const onSubmit = () => {
    unsetModal()
    try {
      doDelete()
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
        className="relative flex flex-col gap-2 rounded-2xl w-96 px-4 p-2 pointer-events-auto before:absolute before:inset-0 before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
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
          // Consider not calling `onSubmit()` here to make it harder to accidentally
          // delete an important asset.
          onSubmit()
        }}
      >
        <div className="relative">Are you sure you want to delete {description}?</div>
        <div className="relative flex gap-2">
          <button
            type="submit"
            className="hover:cursor-pointer inline-block text-white bg-delete rounded-full px-4 py-1"
          >
            Delete
          </button>
          <button
            type="button"
            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1"
            onClick={unsetModal}
          >
            Cancel
          </button>
        </div>
      </form>
    </Modal>
  )
}
