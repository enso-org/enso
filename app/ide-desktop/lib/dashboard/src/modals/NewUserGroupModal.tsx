/** @file A modal to create a user group. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import UnstyledButton from '#/components/UnstyledButton'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'

// =========================
// === NewUserGroupModal ===
// =========================

/** Props for a {@link NewUserGroupModal}. */
export interface NewUserGroupModalProps {
  readonly backend: Backend
  readonly event?: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly userGroups: backendModule.UserGroupInfo[] | null
  readonly onSubmit: (name: string) => void
  readonly onSuccess: (value: backendModule.UserGroupInfo) => void
  readonly onFailure: () => void
}

/** A modal to create a user group. */
export default function NewUserGroupModal(props: NewUserGroupModalProps) {
  const { backend, userGroups: userGroupsRaw, onSubmit: onSubmitRaw, onSuccess, onFailure } = props
  const { event: positionEvent } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [name, setName] = React.useState('')
  const [userGroups, setUserGroups] = React.useState(userGroupsRaw)
  const userGroupNames = React.useMemo(
    () =>
      userGroups == null
        ? null
        : new Set(userGroups.map(group => string.normalizeName(group.groupName))),
    [userGroups]
  )
  const nameError =
    userGroupNames != null && userGroupNames.has(string.normalizeName(name))
      ? getText('duplicateUserGroupError')
      : null
  const canSubmit = nameError == null && name !== '' && userGroupNames != null

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
    <Modal
      centered={positionEvent == null}
      className={`bg-dim ${positionEvent == null ? '' : 'absolute size-full overflow-hidden'}`}
    >
      <form
        data-testid="new-user-group-modal"
        tabIndex={-1}
        className="pointer-events-auto relative flex w-new-label-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        style={positionEvent == null ? {} : { left: positionEvent.pageX, top: positionEvent.pageY }}
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
        <aria.TextField
          className="relative flex flex-col"
          value={name}
          onChange={setName}
          isInvalid={nameError != null}
        >
          <div className="flex items-center">
            <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
            <aria.Input
              autoFocus
              size={1}
              placeholder={getText('userGroupNamePlaceholder')}
              className="text grow rounded-full border border-primary/10 bg-transparent px-input-x invalid:border-red-700/60"
            />
          </div>
          <aria.FieldError className="text-red-700/90">{nameError}</aria.FieldError>
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
