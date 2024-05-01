;

/** @file A row of the user groups table representing a user. */
import * as React from 'react';



import Cross2 from 'enso-assets/cross2.svg';



import * as modalProvider from '#/providers/ModalProvider';
import * as textProvider from '#/providers/TextProvider';



import * as aria from '#/components/aria';
import ContextMenu from '#/components/ContextMenu';
import ContextMenuEntry from '#/components/ContextMenuEntry';
import ContextMenus from '#/components/ContextMenus';
import UnstyledButton from '#/components/UnstyledButton';



import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal';



import * as backend from '#/services/Backend';





;

































;
































// ========================
// === UserGroupUserRow ===
// ========================

/** Props for a {@link UserGroupUserRow}. */
export interface UserGroupUserRowProps {
  readonly user: backend.User
  readonly userGroup: backend.UserGroupInfo
  readonly doRemoveUserFromUserGroup: (
    user: backend.User,
    userGroup: backend.UserGroupInfo
  ) => Promise<void> | void
}

/**  A row of the user groups table representing a user. */
export default function UserGroupUserRow(props: UserGroupUserRowProps) {
  const { user, userGroup, doRemoveUserFromUserGroup } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const cleanupRef = React.useRef(() => {})

  return (
    <aria.Row
      id={`_key-${userGroup.id}-${user.userId}`}
      className="group h-row rounded-rows-child"
      ref={row => {
        cleanupRef.current()
        if (row == null) {
          cleanupRef.current = () => {}
        } else {
          const onContextMenu = (event: MouseEvent) => {
            event.preventDefault()
            event.stopPropagation()
            let position = { pageX: event.pageX, pageY: event.pageY }
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
                          actionText={getText(
                            'removeUserFromUserGroupActionText',
                            user.name,
                            userGroup.groupName
                          )}
                          doDelete={() => {
                            void doRemoveUserFromUserGroup(user, userGroup)
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
      <aria.Cell className="text border-x-2 border-transparent bg-clip-padding rounded-rows-skip-level last:border-r-0">
        <div className="ml-indent-1 flex h-row min-w-max items-center whitespace-nowrap rounded-full">
          <aria.Text className="grow overflow-hidden text-ellipsis whitespace-nowrap px-name-column-x py-name-column-y">
            {user.name}
          </aria.Text>
        </div>
      </aria.Cell>
      <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
        <UnstyledButton
          onPress={() => {
            setModal(
              <ConfirmDeleteModal
                actionText={getText(
                  'removeUserFromUserGroupActionText',
                  user.name,
                  userGroup.groupName
                )}
                actionButtonLabel={getText('remove')}
                doDelete={() => {
                  void doRemoveUserFromUserGroup(user, userGroup)
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