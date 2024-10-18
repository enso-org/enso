/** @file A modal to create a user group. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import { Button, ButtonGroup } from '#/components/AriaComponents'
import Modal from '#/components/Modal'

import type Backend from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =========================
// === NewUserGroupModal ===
// =========================

/** Props for a {@link NewUserGroupModal}. */
export interface NewUserGroupModalProps {
  readonly backend: Backend
  readonly event?: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

/** A modal to create a user group. */
export default function NewUserGroupModal(props: NewUserGroupModalProps) {
  const { backend, event: positionEvent } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [name, setName] = React.useState('')
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const userGroups = listUserGroupsQuery.data ?? null
  const userGroupNames = React.useMemo(
    () =>
      userGroups == null ? null : (
        new Set(userGroups.map((group) => string.normalizeName(group.groupName)))
      ),
    [userGroups],
  )
  const nameError =
    userGroupNames != null && userGroupNames.has(string.normalizeName(name)) ?
      getText('duplicateUserGroupError')
    : null
  const createUserGroup = useMutation(
    backendMutationOptions(backend, 'createUserGroup'),
  ).mutateAsync
  const canSubmit = nameError == null && name !== '' && userGroupNames != null

  const onSubmit = async () => {
    if (canSubmit) {
      unsetModal()
      try {
        await createUserGroup([{ name }])
      } catch (error) {
        toastAndLog(null, error)
      }
    }
  }

  return (
    <Modal
      centered={positionEvent == null}
      className={tailwindMerge.twMerge(
        'bg-dim',
        positionEvent != null && 'absolute size-full overflow-hidden',
      )}
    >
      <form
        tabIndex={-1}
        className="pointer-events-auto relative flex w-new-label-modal flex-col gap-modal rounded-default p-modal-wide pb-3 pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        style={positionEvent == null ? {} : { left: positionEvent.pageX, top: positionEvent.pageY }}
        onKeyDown={(event) => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
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
        <ButtonGroup className="relative">
          <Button variant="submit" isDisabled={!canSubmit} onPress={eventModule.submitForm}>
            {getText('create')}
          </Button>
          <Button variant="outline" onPress={unsetModal}>
            {getText('cancel')}
          </Button>
        </ButtonGroup>
      </form>
    </Modal>
  )
}
