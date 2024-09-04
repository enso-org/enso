/** @file A row of the user groups table representing a user. */
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

// ========================
// === UserGroupUserRow ===
// ========================

/** Props for a {@link UserGroupUserRow}. */
export interface UserGroupUserRowProps {
  readonly user: backend.User
  readonly userGroup: backend.UserGroupInfo
  readonly doRemoveUserFromUserGroup: (user: backend.User, userGroup: backend.UserGroupInfo) => void
}

/** A row of the user groups table representing a user. */
export default function UserGroupUserRow(props: UserGroupUserRowProps) {
  const { user, userGroup, doRemoveUserFromUserGroup } = props
  const { user: currentUser } = useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const isAdmin = currentUser.isOrganizationAdmin
  const contextMenuRef = contextMenuHooks.useContextMenuRef(
    user.userId,
    getText('userGroupUserContextMenuLabel'),
    () => (
      <ContextMenuEntry
        action="delete"
        doAction={() => {
          setModal(
            <ConfirmDeleteModal
              defaultOpen
              actionText={getText(
                'removeUserFromUserGroupActionText',
                user.name,
                userGroup.groupName,
              )}
              doDelete={() => {
                doRemoveUserFromUserGroup(user, userGroup)
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
      id={`_key-${userGroup.id}-${user.userId}`}
      className="group h-row select-none rounded-rows-child"
      ref={contextMenuRef}
    >
      <aria.Cell className="border-x-2 border-transparent bg-clip-padding py-0 rounded-rows-skip-level last:border-r-0">
        <div className="ml-6 flex h-row items-center justify-center rounded-full px-cell-x">
          <ariaComponents.Text nowrap truncate="1" weight="semibold">
            {user.name}
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
              actionText={getText(
                'removeUserFromUserGroupActionText',
                user.name,
                userGroup.groupName,
              )}
              actionButtonLabel={getText('remove')}
              doDelete={() => {
                doRemoveUserFromUserGroup(user, userGroup)
              }}
            />
          </ariaComponents.DialogTrigger>
        )}
      </aria.Cell>
    </aria.Row>
  )
}
