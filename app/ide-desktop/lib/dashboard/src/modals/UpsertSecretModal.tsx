/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import UnstyledButton from '#/components/styled/UnstyledButton'

import type * as backend from '#/services/Backend'

// =========================
// === UpsertSecretModal ===
// =========================

/** Props for a {@link UpsertSecretModal}. */
export interface UpsertSecretModalProps {
  readonly id: backend.SecretId | null
  readonly name: string | null
  readonly doCreate: (name: string, value: string) => void
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretModal(props: UpsertSecretModalProps) {
  const { id, name: nameRaw, doCreate } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()

  const [name, setName] = React.useState(nameRaw ?? '')
  const [value, setValue] = React.useState('')
  const isCreatingSecret = id == null
  const isNameEditable = nameRaw == null
  const canSubmit = Boolean(name && value)

  const doSubmit = () => {
    unsetModal()
    try {
      doCreate(name, value)
    } catch (error) {
      toastAndLog(null, error)
    }
  }

  return (
    <Modal centered className="bg-dim">
      <form
        data-testid="upsert-secret-modal"
        tabIndex={-1}
        className="pointer-events-auto relative flex w-upsert-secret-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
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
          doSubmit()
        }}
      >
        <aria.Heading className="relative text-sm font-semibold">
          {isCreatingSecret ? 'New Secret' : 'Edit Secret'}
        </aria.Heading>
        <aria.TextField className="relative flex h-row items-center">
          <aria.Label className="text w-modal-label">Name</aria.Label>
          <aria.Input
            autoFocus
            disabled={!isNameEditable}
            placeholder="Enter the name of the secret"
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x selectable enabled:active"
            value={name}
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </aria.TextField>
        <aria.TextField className="relative flex h-row items-center">
          <aria.Label className="text w-modal-label">Value</aria.Label>
          <aria.Input
            autoFocus={!isNameEditable}
            placeholder={isNameEditable ? 'Enter the value of the secret' : '●●●●●●●●'}
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x"
            onInput={event => {
              setValue(event.currentTarget.value)
            }}
          />
        </aria.TextField>
        <ButtonRow>
          <UnstyledButton
            isDisabled={!canSubmit}
            className="button bg-invite text-white"
            onPress={doSubmit}
          >
            {isCreatingSecret ? 'Create' : 'Update'}
          </UnstyledButton>
          <UnstyledButton className="button bg-selected-frame" onPress={unsetModal}>
            Cancel
          </UnstyledButton>
        </ButtonRow>
      </form>
    </Modal>
  )
}
