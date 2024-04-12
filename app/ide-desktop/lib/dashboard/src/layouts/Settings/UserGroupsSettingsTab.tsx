/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [userGroups, setUserGroups] = React.useState<backendModule.UserGroupInfo[] | null>(null)

  const [users, setUsers] = React.useState<backendModule.User[] | null>(null)
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
  const isLoading = userGroups == null || users == null

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
              const groups = newUser.userGroups ?? []
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
    <div className="flex h min-h-full flex-1 flex-col gap-settings-section overflow-auto lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection lg:min-w">
        <SettingsSection noFocusArea title={getText('userGroups')} className="overflow-auto">
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                const placeholderId = backendModule.newPlaceholderUserGroupId()
                setModal(
                  <NewUserGroupModal
                    userGroups={userGroups}
                    onSubmit={name => {
                      if (user != null) {
                        setUserGroups(oldUserGroups => [
                          ...(oldUserGroups ?? []),
                          {
                            organizationId: user.organizationId,
                            id: placeholderId,
                            groupName: name,
                          },
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
          <aria.Table
            aria-label={getText('userGroups')}
            className="table-fixed self-start rounded-rows"
            dragAndDropHooks={dragAndDropHooks}
          >
            <aria.TableHeader className="h-row">
              {/* Delete button. */}
              <aria.Column className="relative border-0" />
              <aria.Column
                isRowHeader
                className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
              >
                {getText('userGroup')}
              </aria.Column>
            </aria.TableHeader>
            <aria.TableBody
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
                    className="bg-transparent"
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
                    <aria.Row
                      id={userGroup.id}
                      className={`group h-row ${backendModule.isPlaceholderUserGroupId(userGroup.id) ? 'pointer-events-none placeholder' : ''}`}
                    >
                      <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
                        <UnstyledButton
                          onPress={() => {
                            void doDeleteUserGroup(userGroup)
                          }}
                          className="absolute right-full size-icon -translate-y-1/2"
                        >
                          <img src={Cross2} className="size-icon" />
                        </UnstyledButton>
                      </aria.Cell>
                      <aria.Cell className="text rounded-l-full border-x-2 border-transparent bg-clip-padding px-cell-x last:rounded-r-full last:border-r-0">
                        {userGroup.groupName}
                      </aria.Cell>
                    </aria.Row>
                    {(usersByGroup.get(userGroup.id) ?? []).map(otherUser => (
                      <aria.Row
                        key={otherUser.userId}
                        id={`${userGroup.id}-${otherUser.userId}`}
                        className="group h-row"
                      >
                        <aria.Cell className="relative bg-transparent p transparent group-hover-2:opacity-100">
                          <UnstyledButton
                            onPress={() => {
                              void doRemoveUserFromUserGroup(otherUser, userGroup)
                            }}
                            className="absolute right-full size-icon -translate-y-1/2 translate-x-indent-1"
                          >
                            <img src={Cross2} className="size-icon" />
                          </UnstyledButton>
                        </aria.Cell>
                        <aria.Cell className="text border-x-2 border-transparent bg-clip-padding rounded-rows-skip-level last:border-r-0">
                          <div className="ml-indent-1 flex h-row min-w-max items-center whitespace-nowrap rounded-full">
                            <aria.Text className="grow px-name-column-x py-name-column-y">
                              {otherUser.name}
                            </aria.Text>
                          </div>
                        </aria.Cell>
                      </aria.Row>
                    ))}
                  </>
                )
              )}
            </aria.TableBody>
          </aria.Table>
        </SettingsSection>
      </div>
      <SettingsSection noFocusArea title={getText('users')}>
        <MembersTable draggable />
      </SettingsSection>
    </div>
  )
}
