/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

// ==============================
// === ConfirmDeleteUserModal ===
// ==============================

/** A modal for confirming the deletion of a user. */
export default function ConfirmDeleteUserModal() {
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()
  const { signOut } = authProvider.useAuth()

  const onSubmit = async () => {
    unsetModal()
    try {
      await backend.deleteUser()
      await signOut()
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
        className="relative flex flex-col items-center gap-2 rounded-2xl w-96 px-4 p-2 pointer-events-auto before:absolute before:inset-0 before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
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
          Once deleted, this user account will be gone forever.
          <button type="submit" className="rounded-full bg-danger text-inversed px-2 py-1">
            <span className="leading-5 h-6 py-px">
              I confirm that I want to delete this user account.
            </span>
          </button>
        </div>
      </form>
    </Modal>
  )
}
