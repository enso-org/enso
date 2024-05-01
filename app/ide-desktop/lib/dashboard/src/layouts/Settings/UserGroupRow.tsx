/** @file A row representing a user group. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as tooltipHooks from '#/hooks/tooltipHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'
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
  const cleanupRef = React.useRef(() => {})
  const { needsTooltip, tooltipTargetRef } = tooltipHooks.useNeedsTooltip()

  return (
    <aria.Row
      id={userGroup.id}
      className={`group h-row rounded-rows-child ${backend.isPlaceholderUserGroupId(userGroup.id) ? 'pointer-events-none placeholder' : ''}`}
      ref={row => {
        cleanupRef.current()
        if (row == null) {
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
                key={userGroup.id}
                event={event}
              >
                <ContextMenu aria-label={getText('userGroupContextMenuLabel')}>
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
        ref={tooltipTargetRef}
        className="text rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0"
      >
        <aria.TooltipTrigger>
          {/* NOTE: `max-w-full` brings back the ellipsis, but the tooltip disappears */}
          <aria.Button className="cursor-default overflow-hidden text-ellipsis whitespace-nowrap">
            {userGroup.groupName}
          </aria.Button>
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
          className="absolute left-full size-icon -translate-y-1/2"
        >
          <img src={Cross2} className="size-icon" />
        </UnstyledButton>
      </aria.Cell>
    </aria.Row>
  )
}
