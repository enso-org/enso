/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as mimeTypes from '#/data/mimeTypes'

import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'
import UserGroupRow from '#/layouts/Settings/UserGroupRow'
import UserGroupUserRow from '#/layouts/Settings/UserGroupUserRow'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as object from '#/utilities/object'

// =============================
// === UserGroupsSettingsTab ===
// =============================

/** Props for a {@link UserGroupsSettingsTab}. */
export interface UserGroupsSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function UserGroupsSettingsTab(props: UserGroupsSettingsTabProps) {
  const { backend } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [userGroups, setUserGroups] = React.useState<backendModule.UserGroupInfo[] | null>(null)
  const [users, setUsers] = React.useState<backendModule.User[] | null>(null)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const isLoading = userGroups == null || users == null
  const usersMap = React.useMemo(
    () => new Map((users ?? []).map(otherUser => [otherUser.userId, otherUser])),
    [users]
  )

  const usersByGroup = React.useMemo(() => {
    const map = new Map<backendModule.UserGroupId, backendModule.User[]>()
    for (const otherUser of users ?? []) {
      for (const userGroupId of otherUser.userGroups ?? []) {
        let userList = map.get(userGroupId)
        if (userList == null) {
          userList = []
          map.set(userGroupId, userList)
        }
        userList.push(otherUser)
      }
    }
    return map
  }, [users])

  const { onScroll: onUserGroupsTableScroll, shadowClass } =
    scrollHooks.useStickyTableHeaderOnScroll(rootRef, bodyRef, true)

  React.useEffect(() => {
    void backend.listUsers().then(setUsers)
    void backend.listUserGroups().then(setUserGroups)
  }, [backend])

  const { dragAndDropHooks } = aria.useDragAndDrop({
    getDropOperation: (target, types, allowedOperations) =>
      allowedOperations.includes('copy') &&
      types.has(mimeTypes.USER_MIME_TYPE) &&
      target.type === 'item' &&
      typeof target.key === 'string' &&
      backendModule.isUserGroupId(target.key) &&
      !backendModule.isPlaceholderUserGroupId(target.key)
        ? 'copy'
        : 'cancel',
    onItemDrop: event => {
      if (typeof event.target.key === 'string' && backendModule.isUserGroupId(event.target.key)) {
        const userGroupId = event.target.key
        for (const item of event.items) {
          if (item.kind === 'text' && item.types.has(mimeTypes.USER_MIME_TYPE)) {
            void item.getText(mimeTypes.USER_MIME_TYPE).then(async text => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const newUser: backendModule.User = JSON.parse(text)
              const groups = usersMap.get(newUser.userId)?.userGroups ?? []
              if (!groups.includes(userGroupId)) {
                try {
                  const newUserGroups = [...groups, userGroupId]
                  setUsers(
                    oldUsers =>
                      oldUsers?.map(otherUser =>
                        otherUser.userId !== newUser.userId
                          ? otherUser
                          : object.merge(otherUser, { userGroups: newUserGroups })
                      ) ?? null
                  )
                  await backend.changeUserGroup(
                    newUser.userId,
                    { userGroups: newUserGroups },
                    newUser.name
                  )
                } catch (error) {
                  toastAndLog('changeUserGroupsError', error)
                  setUsers(
                    oldUsers =>
                      oldUsers?.map(otherUser =>
                        otherUser.userId !== newUser.userId
                          ? otherUser
                          : object.merge(otherUser, {
                              userGroups:
                                otherUser.userGroups?.filter(id => id !== userGroupId) ?? null,
                            })
                      ) ?? null
                  )
                }
              }
            })
          }
        }
      }
    },
  })

  const doDeleteUserGroup = async (userGroup: backendModule.UserGroupInfo) => {
    setUsers(
      oldUsers =>
        oldUsers?.map(otherUser =>
          otherUser.userGroups?.includes(userGroup.id) !== true
            ? otherUser
            : object.merge(otherUser, {
                userGroups: otherUser.userGroups.filter(
                  userGroupId => userGroupId !== userGroup.id
                ),
              })
        ) ?? null
    )
    setUserGroups(oldUserGroups => {
      const newUserGroups =
        oldUserGroups?.filter(otherUserGroup => otherUserGroup.id !== userGroup.id) ?? null
      return newUserGroups?.length === 0 ? null : newUserGroups
    })
    try {
      await backend.deleteUserGroup(userGroup.id, userGroup.groupName)
    } catch (error) {
      toastAndLog('deleteUserGroupError', error, userGroup.groupName)
      const usersInGroup = usersByGroup.get(userGroup.id)
      setUserGroups(oldUserGroups => [
        ...(oldUserGroups?.filter(otherUserGroup => otherUserGroup.id !== userGroup.id) ?? []),
        userGroup,
      ])
      if (usersInGroup != null) {
        const userIds = new Set(usersInGroup.map(otherUser => otherUser.userId))
        setUsers(
          oldUsers =>
            oldUsers?.map(oldUser =>
              !userIds.has(oldUser.userId) || oldUser.userGroups?.includes(userGroup.id) === true
                ? oldUser
                : object.merge(oldUser, {
                    userGroups: [...(oldUser.userGroups ?? []), userGroup.id],
                  })
            ) ?? null
        )
      }
    }
  }

  const doRemoveUserFromUserGroup = async (
    otherUser: backendModule.User,
    userGroup: backendModule.UserGroupInfo
  ) => {
    try {
      const intermediateUserGroups =
        otherUser.userGroups?.filter(userGroupId => userGroupId !== userGroup.id) ?? null
      const newUserGroups = intermediateUserGroups?.length === 0 ? null : intermediateUserGroups
      setUsers(
        oldUsers =>
          oldUsers?.map(oldUser =>
            oldUser.userId !== otherUser.userId
              ? oldUser
              : object.merge(otherUser, { userGroups: newUserGroups })
          ) ?? null
      )
      await backend.changeUserGroup(
        otherUser.userId,
        { userGroups: newUserGroups ?? [] },
        otherUser.name
      )
    } catch (error) {
      toastAndLog('removeUserFromUserGroupError', error, otherUser.name, userGroup.groupName)
      setUsers(
        oldUsers =>
          oldUsers?.map(oldUser =>
            oldUser.userId !== otherUser.userId
              ? oldUser
              : object.merge(otherUser, {
                  userGroups: [...(oldUser.userGroups ?? []), userGroup.id],
                })
          ) ?? null
      )
    }
  }

  return (
    <div className="flex h min-h-full flex-1 flex-col gap-settings-section overflow-hidden lg:h-auto lg:flex-row">
      <div className="flex h-3/5 w-settings-main-section max-w-full flex-col gap-settings-subsection lg:h-[unset] lg:min-w">
        <SettingsSection noFocusArea title={getText('userGroups')} className="overflow-hidden">
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={event => {
                const placeholderId = backendModule.newPlaceholderUserGroupId()
                const rect = event.target.getBoundingClientRect()
                const position = { pageX: rect.left, pageY: rect.top }
                setModal(
                  <NewUserGroupModal
                    backend={backend}
                    event={position}
                    userGroups={userGroups}
                    onSubmit={groupName => {
                      if (user != null) {
                        const id = placeholderId
                        const { organizationId } = user
                        setUserGroups(oldUserGroups => [
                          ...(oldUserGroups ?? []),
                          { organizationId, id, groupName },
                        ])
                      }
                    }}
                    onSuccess={newUserGroup => {
                      setUserGroups(
                        oldUserGroups =>
                          oldUserGroups?.map(userGroup =>
                            userGroup.id !== placeholderId ? userGroup : newUserGroup
                          ) ?? null
                      )
                    }}
                    onFailure={() => {
                      setUserGroups(
                        oldUserGroups =>
                          oldUserGroups?.filter(userGroup => userGroup.id !== placeholderId) ?? null
                      )
                    }}
                  />
                )
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('newUserGroup')}
              </aria.Text>
            </UnstyledButton>
          </HorizontalMenuBar>
          <div
            ref={rootRef}
            className={`overflow-auto overflow-x-hidden transition-all lg:mb-2 ${shadowClass}`}
            onScroll={onUserGroupsTableScroll}
          >
            <aria.Table
              aria-label={getText('userGroups')}
              className="w-full table-fixed self-start rounded-rows"
              dragAndDropHooks={dragAndDropHooks}
            >
              <aria.TableHeader className="sticky top h-row">
                <aria.Column
                  isRowHeader
                  className="w-full border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
                >
                  {getText('userGroup')}
                </aria.Column>
                {/* Delete button. */}
                <aria.Column className="relative border-0" />
              </aria.TableHeader>
              <aria.TableBody
                ref={bodyRef}
                items={userGroups ?? []}
                dependencies={[isLoading, userGroups, usersByGroup]}
                className="select-text"
              >
                {isLoading ? (
                  <aria.Row className="h-row">
                    <aria.Cell
                      ref={element => {
                        if (element != null) {
                          element.colSpan = 2
                        }
                      }}
                    >
                      <div className="flex justify-center">
                        <StatelessSpinner
                          size={32}
                          state={statelessSpinner.SpinnerState.loadingMedium}
                        />
                      </div>
                    </aria.Cell>
                  </aria.Row>
                ) : (
                  userGroup => (
                    <>
                      <UserGroupRow userGroup={userGroup} doDeleteUserGroup={doDeleteUserGroup} />
                      {(usersByGroup.get(userGroup.id) ?? []).map(otherUser => (
                        <UserGroupUserRow
                          key={otherUser.userId}
                          user={otherUser}
                          userGroup={userGroup}
                          doRemoveUserFromUserGroup={doRemoveUserFromUserGroup}
                        />
                      ))}
                    </>
                  )
                )}
              </aria.TableBody>
            </aria.Table>
          </div>
        </SettingsSection>
      </div>
      <SettingsSection noFocusArea title={getText('users')} className="h-2/5 lg:h-[unset]">
        <MembersTable draggable populateWithSelf backend={backend} />
      </SettingsSection>
    </div>
  )
}
