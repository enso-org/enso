/** @file A row representing a user in a table of users. */
import Cross2 from '#/assets/cross2.svg'

import * as contextMenuHooks from '#/hooks/contextMenuHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import type * as backend from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

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
  const { user: self } = authProvider.useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const isAdmin = self.isOrganizationAdmin
  const isSelf = user.userId === self.userId
  const doDeleteUser = isSelf ? null : doDeleteUserRaw

  const contextMenuRef = contextMenuHooks.useContextMenuRef(
    user.userId,
    getText('userContextMenuLabel'),
    () =>
      doDeleteUser == null ? null : (
        <ContextMenuEntry
          action="delete"
          doAction={() => {
            setModal(
              <ConfirmDeleteModal
                defaultOpen
                actionText={getText('deleteUserActionText', user.name)}
                doDelete={() => {
                  doDeleteUser(user)
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
      id={user.userId}
      className={tailwindMerge.twMerge(
        'group h-row rounded-rows-child',
        draggable && 'cursor-grab',
      )}
      ref={contextMenuRef}
    >
      <aria.Cell className="text relative overflow-hidden whitespace-nowrap border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0 group-selected:bg-selected-frame">
        {draggable && (
          <aria.FocusRing>
            <aria.Button
              slot="drag"
              className="absolute left top-1/2 ml-1 h-2 w-2 -translate-y-1/2 rounded-sm"
            />
          </aria.FocusRing>
        )}

        <div className="flex justify-center">
          <ariaComponents.Text nowrap truncate="1" weight="semibold">
            {user.name}
          </ariaComponents.Text>
        </div>
      </aria.Cell>
      <aria.Cell className="text whitespace-nowrap rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0 group-selected:bg-selected-frame">
        <ariaComponents.Text nowrap truncate="1" className="block">
          {user.email}
        </ariaComponents.Text>
      </aria.Cell>
      {doDeleteUserRaw == null ?
        null
      : doDeleteUser == null ?
        <></>
      : <aria.Cell className="relative bg-transparent p-0 opacity-0 group-hover-2:opacity-100">
          <ariaComponents.DialogTrigger>
            <ariaComponents.Button
              size="custom"
              variant="custom"
              className="absolute right-full mr-4 size-4 -translate-y-1/2"
            >
              <img src={Cross2} className="size-4" />
            </ariaComponents.Button>
            <ConfirmDeleteModal
              actionText={getText('deleteUserActionText', user.name)}
              doDelete={() => {
                doDeleteUser(user)
              }}
            />
          </ariaComponents.DialogTrigger>
        </aria.Cell>
      }
    </aria.Row>
  )
}
