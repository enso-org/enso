/** @file A modal to create a user group. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import UnstyledButton from '#/components/UnstyledButton'

import type * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'

// =========================
// === NewUserGroupModal ===
// =========================

/** Props for a {@link NewUserGroupModal}. */
export interface NewUserGroupModalProps {
  readonly userGroups: backendModule.UserGroupInfo[] | null
  readonly onSubmit: (name: string) => void
  readonly onSuccess: (value: backendModule.UserGroupInfo) => void
  readonly onFailure: () => void
}

/** A modal to create a user group. */
export default function NewUserGroupModal(props: NewUserGroupModalProps) {
  const { userGroups: userGroupsRaw, onSubmit: onSubmitRaw, onSuccess, onFailure } = props
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [name, setName] = React.useState('')
  const [userGroups, setUserGroups] = React.useState(userGroupsRaw)
  const userGroupNames = React.useMemo(
    () => (userGroups == null ? null : new Set(userGroups.map(group => group.groupName))),
    [userGroups]
  )
  const canSubmit = name !== '' && userGroupNames != null && !userGroupNames.has(name)

  React.useEffect(() => {
    if (userGroups == null) {
      void backend.listUserGroups().then(setUserGroups)
    }
  }, [backend, userGroups])

  const onSubmit = async () => {
    if (canSubmit) {
      unsetModal()
      try {
        onSubmitRaw(name)
        onSuccess(await backend.createUserGroup({ name }))
      } catch (error) {
        toastAndLog(null, error)
        onFailure()
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
        <aria.Heading className="relative text-sm font-semibold">
          {getText('newUserGroup')}
        </aria.Heading>
        <aria.TextField className="relative flex items-center" value={name} onChange={setName}>
          <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
          <aria.Input
            autoFocus
            size={1}
            placeholder={getText('userGroupNamePlaceholder')}
            className="text grow rounded-full border border-primary/10 bg-transparent px-input-x"
          />
        </aria.TextField>
        <ButtonRow>
          <UnstyledButton
            isDisabled={!canSubmit}
            className="button bg-invite text-white enabled:active"
            onPress={eventModule.submitForm}
          >
            {getText('create')}
          </UnstyledButton>
          <UnstyledButton className="button bg-selected-frame active" onPress={unsetModal}>
            {getText('cancel')}
          </UnstyledButton>
        </ButtonRow>
      </form>
    </Modal>
  )
}
