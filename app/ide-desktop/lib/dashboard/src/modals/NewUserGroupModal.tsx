/** @file A modal to create a user group. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Modal from '#/components/Modal'

// =========================
// === NewUserGroupModal ===
// =========================

/** Props for a {@link NewUserGroupModal}. */
export interface NewUserGroupModalProps {
  readonly onSubmit: () => void
}

/** A modal to create a user group. */
export default function NewUserGroupModal(props: NewUserGroupModalProps) {
  const { onSubmit: onSubmitRaw } = props
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [name, setName] = React.useState('')
  const userGroups = asyncEffectHooks.useAsyncEffect(null, () => backend.listUserGroups(), [])
  const userGroupNames = React.useMemo(
    () => (userGroups == null ? null : new Set(userGroups.map(group => group.groupName))),
    [userGroups]
  )
  const canSubmit = name !== '' && userGroupNames != null && !userGroupNames.has(name)

  const onSubmit = async () => {
    if (canSubmit) {
      unsetModal()
      try {
        await backend.createUserGroup({ name })
        onSubmitRaw()
      } catch (error) {
        toastAndLog(null, error)
      }
    }
  }

  return (
    <Modal centered className="absolute bg-dim">
      <form
        data-testid="new-user-group-modal"
        tabIndex={-1}
        className="pointer-events-auto relative flex w-new-label-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
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
        <h1 className="relative text-sm font-semibold">{getText('newUserGroup')}</h1>
        <label className="relative flex items-center">
          <div className="text w-modal-label">{getText('name')}</div>
          <input
            autoFocus
            size={1}
            placeholder={getText('userGroupNamePlaceholder')}
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x"
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </label>
        <div className="relative flex gap-buttons">
          <button
            disabled={!canSubmit}
            type="submit"
            className="button bg-invite text-white enabled:active"
          >
            {getText('create')}
          </button>
          <button type="button" className="button bg-selected-frame active" onClick={unsetModal}>
            {getText('cancel')}
          </button>
        </div>
      </form>
    </Modal>
  )
}
