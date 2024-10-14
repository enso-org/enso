/** @file A row representing a user group. */
import Cross2 from '#/assets/cross2.svg'

import * as contextMenuHooks from '#/hooks/contextMenuHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import type * as backend from '#/services/Backend'

import { useFullUserSession } from '#/providers/AuthProvider'

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
  const { user } = useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const isAdmin = user.isOrganizationAdmin
  const contextMenuRef = contextMenuHooks.useContextMenuRef(
    userGroup.id,
    getText('userGroupContextMenuLabel'),
    () => (
      <ContextMenuEntry
        action="delete"
        doAction={() => {
          setModal(
            <ConfirmDeleteModal
              defaultOpen
              actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
              doDelete={() => {
                doDeleteUserGroup(userGroup)
              }}
            />,
          )
        }}
      />
    ),
    { enabled: isAdmin },
  )

  return (
    <aria.Row
      id={userGroup.id}
      className="group h-row select-none rounded-rows-child"
      ref={contextMenuRef}
    >
      <aria.Cell className="rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0">
        <div className="flex justify-center">
          <ariaComponents.Text nowrap truncate="1" weight="semibold">
            {userGroup.groupName}
          </ariaComponents.Text>
        </div>
      </aria.Cell>
      <aria.Cell className="relative bg-transparent p-0 opacity-0 group-hover-2:opacity-100">
        {isAdmin && (
          <ariaComponents.DialogTrigger>
            <ariaComponents.Button
              size="custom"
              variant="custom"
              className="absolute right-full mr-4 size-4 -translate-y-1/2"
            >
              <img src={Cross2} className="size-4" />
            </ariaComponents.Button>
            <ConfirmDeleteModal
              actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
              doDelete={() => {
                doDeleteUserGroup(userGroup)
              }}
            />
          </ariaComponents.DialogTrigger>
        )}
      </aria.Cell>
    </aria.Row>
  )
}
