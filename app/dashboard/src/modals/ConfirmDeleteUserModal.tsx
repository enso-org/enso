/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Modal from '#/components/Modal'

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
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

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
        ref={(element) => {
          element?.focus()
        }}
        tabIndex={-1}
        className="pointer-events-auto relative flex w-confirm-delete-user-modal flex-col items-center gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
          void doSubmit()
        }}
      >
        <aria.Heading className="py-heading relative h-heading text-xl font-bold">
          {getText('areYouSure')}
        </aria.Heading>
        <aria.Text className="relative mb-2 text-balance text-center">
          {getText('confirmDeleteUserAccountWarning')}
        </aria.Text>
        <ariaComponents.ButtonGroup className="relative self-center">
          <ariaComponents.Button
            size="custom"
            variant="custom"
            className="button relative bg-danger text-inversed active"
            onPress={doSubmit}
          >
            <aria.Text className="text">{getText('confirmDeleteUserAccountButtonLabel')}</aria.Text>
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}
