/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Modal from '#/components/Modal'

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
  const { getText } = textProvider.useText()

  const [name, setName] = React.useState(nameRaw ?? '')
  const [value, setValue] = React.useState('')
  const isCreatingSecret = id == null
  const isNameEditable = nameRaw == null
  const canSubmit = Boolean(name && value)

  const onSubmit = () => {
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
          onSubmit()
        }}
      >
        <h1 className="relative text-sm font-semibold">
          {isCreatingSecret ? getText('newSecret') : getText('editSecret')}
        </h1>
        <label className="relative flex h-row items-center">
          <div className="text w-modal-label">{getText('name')}</div>
          <input
            autoFocus
            disabled={!isNameEditable}
            placeholder={getText('secretNamePlaceholder')}
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x selectable enabled:active"
            value={name}
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </label>
        <label className="relative flex h-row items-center">
          <div className="text w-modal-label">{getText('value')}</div>
          <input
            autoFocus={!isNameEditable}
            placeholder={
              isNameEditable ? getText('secretValuePlaceholder') : getText('secretValueHidden')
            }
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x"
            onInput={event => {
              setValue(event.currentTarget.value)
            }}
          />
        </label>
        <div className="relative flex gap-buttons">
          <button disabled={!canSubmit} type="submit" className="button bg-invite text-white">
            {isCreatingSecret ? getText('create') : getText('update')}
          </button>
          <button type="button" className="button bg-selected-frame" onClick={unsetModal}>
            {getText('cancel')}
          </button>
        </div>
      </form>
    </Modal>
  )
}
