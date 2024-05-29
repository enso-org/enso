/** @file A row representing a user group. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import Cross2 from 'enso-assets/cross2.svg'

import type * as backendHooks from '#/hooks/backendHooks'
import * as contextMenuHooks from '#/hooks/contextMenuHooks'
import * as tooltipHooks from '#/hooks/tooltipHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import FocusableText from '#/components/FocusableText'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import type * as backend from '#/services/Backend'

// ====================
// === UserGroupRow ===
// ====================

/** Props for a {@link UserGroupRow}. */
export interface UserGroupRowProps {
  readonly userGroup: backendHooks.WithPlaceholder<backend.UserGroupInfo>
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
      className={tailwindMerge.twMerge(
        'group h-row select-none rounded-rows-child',
        userGroup.isPlaceholder && 'pointer-events-none placeholder'
      )}
      ref={contextMenuRef}
    >
      <aria.Cell className="text rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0">
        <ariaComponents.TooltipTrigger>
          <FocusableText
            ref={tooltipTargetRef}
            className="block cursor-default overflow-hidden text-ellipsis whitespace-nowrap"
          >
            {userGroup.groupName}
          </FocusableText>
          {needsTooltip && <ariaComponents.Tooltip>{userGroup.groupName}</ariaComponents.Tooltip>}
        </ariaComponents.TooltipTrigger>
      </aria.Cell>
      <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
        <ariaComponents.Button
          size="custom"
          variant="custom"
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
        </ariaComponents.Button>
      </aria.Cell>
    </aria.Row>
  )
}
