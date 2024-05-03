/** @file A row representing a user in a table of users. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as contextMenuHooks from '#/hooks/contextMenuHooks'
import * as tooltipHooks from '#/hooks/tooltipHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import FocusableText from '#/components/FocusableText'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import type * as backend from '#/services/Backend'

// ===============
// === UserRow ===
// ===============

/** Props for a {@link UserRow}. */
export interface UserRowProps {
  readonly id: string
  readonly draggable?: boolean
  readonly user: backend.User
  readonly doDeleteUser?: ((user: backend.User) => void) | null
}

/** A row representing a user in a table of users. */
export default function UserRow(props: UserRowProps) {
  const { draggable = false, user, doDeleteUser: doDeleteUserRaw } = props
  const { user: self } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { needsTooltip, tooltipTargetRef } = tooltipHooks.useNeedsTooltip()
  const isSelf = user.userId === self?.userId
  const doDeleteUser = isSelf ? null : doDeleteUserRaw
  const contextMenuRef = contextMenuHooks.useContextMenuRef(
    user.userId,
    getText('userContextMenuLabel'),
    position =>
      doDeleteUser == null ? null : (
        <ContextMenuEntry
          action="delete"
          doAction={() => {
            setModal(
              <ConfirmDeleteModal
                event={position}
                actionText={getText('deleteUserActionText', user.name)}
                doDelete={() => {
                  doDeleteUser(user)
                }}
              />
            )
          }}
        />
      )
  )

  return (
    <aria.Row
      id={user.userId}
      className={`group h-row rounded-rows-child ${draggable ? 'cursor-grab' : ''}`}
      ref={contextMenuRef}
    >
      <aria.Cell
        ref={tooltipTargetRef}
        className="text overflow-hidden whitespace-nowrap border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0 group-selected:bg-selected-frame"
      >
        {draggable && <aria.Button slot="drag" />}
        <aria.TooltipTrigger>
          {/* NOTE: `max-w-full` brings back the ellipsis, but the tooltip disappears */}
          <FocusableText className="cursor-[unset] overflow-hidden text-ellipsis whitespace-nowrap">
            {user.name}
          </FocusableText>
          {needsTooltip && <ariaComponents.Tooltip>{user.name}</ariaComponents.Tooltip>}
        </aria.TooltipTrigger>
      </aria.Cell>
      <aria.Cell className="text whitespace-nowrap rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0 group-selected:bg-selected-frame">
        {user.email}
      </aria.Cell>
      {doDeleteUserRaw == null ? null : doDeleteUser == null ? (
        <></>
      ) : (
        <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
          <UnstyledButton
            onPress={event => {
              const rect = event.target.getBoundingClientRect()
              const position = { pageX: rect.left, pageY: rect.top }
              setModal(
                <ConfirmDeleteModal
                  event={position}
                  actionText={getText('deleteUserActionText', user.name)}
                  doDelete={() => {
                    doDeleteUser(user)
                  }}
                />
              )
            }}
            className="absolute left-full size-icon -translate-y-1/2"
          >
            <img src={Cross2} className="size-icon" />
          </UnstyledButton>
        </aria.Cell>
      )}
    </aria.Row>
  )
}
