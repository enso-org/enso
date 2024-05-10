/** @file A row representing a user group. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as contextMenuHooks from '#/hooks/contextMenuHooks'
import * as tooltipHooks from '#/hooks/tooltipHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import FocusableText from '#/components/FocusableText'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as backend from '#/services/Backend'

// ====================
// === UserGroupRow ===
// ====================

/** Props for a {@link UserGroupRow}. */
export interface UserGroupRowProps {
  readonly userGroup: backend.UserGroupInfo
  readonly doDeleteUserGroup: (userGroup: backend.UserGroupInfo) => void
}

/** A row representing a user group. */
export default function UserGroupRow(props: UserGroupRowProps) {
  const { userGroup, doDeleteUserGroup } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { needsTooltip, tooltipTargetRef } = tooltipHooks.useNeedsTooltip()
  const contextMenuRef = contextMenuHooks.useContextMenuRef(
    userGroup.id,
    getText('userGroupContextMenuLabel'),
    position => (
      <ContextMenuEntry
        action="delete"
        doAction={() => {
          setModal(
            <ConfirmDeleteModal
              event={position}
              actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
              doDelete={() => {
                doDeleteUserGroup(userGroup)
              }}
            />
          )
        }}
      />
    )
  )

  return (
    <aria.Row
      id={userGroup.id}
      className={`group h-row rounded-rows-child ${backend.isPlaceholderUserGroupId(userGroup.id) ? 'pointer-events-none placeholder' : ''}`}
      ref={contextMenuRef}
    >
      <aria.Cell className="text rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0">
        <aria.TooltipTrigger>
          <FocusableText
            ref={tooltipTargetRef}
            className="block cursor-unset overflow-hidden text-ellipsis whitespace-nowrap"
          >
            {userGroup.groupName}
          </FocusableText>
          {needsTooltip && <ariaComponents.Tooltip>{userGroup.groupName}</ariaComponents.Tooltip>}
        </aria.TooltipTrigger>
      </aria.Cell>
      <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
        <UnstyledButton
          onPress={() => {
            setModal(
              <ConfirmDeleteModal
                actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
                doDelete={() => {
                  doDeleteUserGroup(userGroup)
                }}
              />
            )
          }}
          className="absolute right-full mr-4 size-icon -translate-y-1/2"
        >
          <img src={Cross2} className="size-icon" />
        </UnstyledButton>
      </aria.Cell>
    </aria.Row>
  )
}
