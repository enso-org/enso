/** @file A row representing a user in a table of users. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'
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
  readonly doDeleteUser?: (user: backend.User) => void
}

/** A row representing a user in a table of users. */
export default function UserRow(props: UserRowProps) {
  const { draggable = false, user, doDeleteUser: doDeleteUserRaw } = props
  const { user: self } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const cleanupRef = React.useRef(() => {})
  const nameCellCleanupRef = React.useRef(() => {})
  const [needsTooltip, setNeedsTooltip] = React.useState(false)
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(changes => {
        for (const change of changes.slice(0, 1)) {
          if (change.target instanceof HTMLElement) {
            setNeedsTooltip(change.target.clientWidth < change.target.scrollWidth)
          }
        }
      })
  )
  const isSelf = user.userId === self?.userId
  const doDeleteUser = isSelf ? null : doDeleteUserRaw

  return (
    <aria.Row
      id={user.userId}
      className={`group h-row rounded-rows-child ${draggable ? 'cursor-grab' : ''}`}
      ref={row => {
        cleanupRef.current()
        if (row == null || doDeleteUser == null) {
          cleanupRef.current = () => {}
        } else {
          const onContextMenu = (event: MouseEvent) => {
            event.preventDefault()
            event.stopPropagation()
            const position = { pageX: event.pageX, pageY: event.pageY }
            setModal(
              <ContextMenus
                ref={element => {
                  if (element != null) {
                    const rect = element.getBoundingClientRect()
                    position.pageX = rect.left
                    position.pageY = rect.top
                  }
                }}
                key={user.userId}
                event={event}
              >
                <ContextMenu aria-label={getText('userContextMenuLabel')}>
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
                </ContextMenu>
              </ContextMenus>
            )
          }
          row.addEventListener('contextmenu', onContextMenu)
          cleanupRef.current = () => {
            row.removeEventListener('contextmenu', onContextMenu)
          }
        }
      }}
    >
      <aria.Cell
        ref={cell => {
          nameCellCleanupRef.current()
          if (cell == null) {
            nameCellCleanupRef.current = () => {}
          } else {
            setNeedsTooltip(cell.clientWidth < cell.scrollWidth)
            resizeObserver.observe(cell)
            nameCellCleanupRef.current = () => {
              resizeObserver.unobserve(cell)
            }
          }
        }}
        className="text overflow-hidden text-ellipsis whitespace-nowrap border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0 group-selected:bg-selected-frame"
      >
        {draggable && <aria.Button slot="drag" />}
        <aria.TooltipTrigger>
          {/* NOTE: `max-w-full` brings back the ellipsis, but the tooltip disappears */}
          <aria.Button className="cursor-default overflow-hidden text-ellipsis whitespace-nowrap">
            {user.name}
          </aria.Button>
          {needsTooltip && <ariaComponents.Tooltip>{user.name}</ariaComponents.Tooltip>}
        </aria.TooltipTrigger>
      </aria.Cell>
      <aria.Cell className="text whitespace-nowrap rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0 group-selected:bg-selected-frame">
        {user.email}
      </aria.Cell>
      {doDeleteUser != null && (
        <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
          <UnstyledButton
            onPress={event => {
              const position = { pageX: event.target.clientLeft, pageY: event.target.clientTop }
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
