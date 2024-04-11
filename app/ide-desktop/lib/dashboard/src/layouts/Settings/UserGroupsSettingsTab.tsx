/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import Cross2 from 'enso-assets/cross2.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as refreshHooks from '#/hooks/refreshHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

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
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const userGroups = asyncEffectHooks.useAsyncEffect(null, () => backend.listUserGroups(), [
    backend,
    refresh,
  ])
  const [users, setUsers] = React.useState<backendModule.User[] | null>(null)
  const usersByGroup = React.useMemo(() => {
    const map = new Map<backendModule.UserGroupId, backendModule.User[]>()
    for (const user of users ?? []) {
      for (const userGroupId of user.userGroups ?? []) {
        let userList = map.get(userGroupId)
        if (userList == null) {
          userList = []
          map.set(userGroupId, userList)
        }
        userList.push(user)
      }
    }
    return map
  }, [users])
  const isLoading = userGroups == null || users == null
  const { dragAndDropHooks } = aria.useDragAndDrop({
    getDropOperation: (target, types, allowedOperations) =>
      allowedOperations.includes('copy') &&
      types.has(mimeTypes.USER_MIME_TYPE) &&
      target.type === 'item' &&
      typeof target.key === 'string' &&
      backendModule.isUserGroupId(target.key)
        ? 'copy'
        : 'cancel',
    onItemDrop: event => {
      if (typeof event.target.key === 'string' && backendModule.isUserGroupId(event.target.key)) {
        const userGroupId = event.target.key
        for (const item of event.items) {
          if (item.kind === 'text' && item.types.has(mimeTypes.USER_MIME_TYPE)) {
            void item.getText(mimeTypes.USER_MIME_TYPE).then(async text => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const user: backendModule.User = JSON.parse(text)
              const groups = user.userGroups ?? []
              if (!groups.includes(userGroupId)) {
                try {
                  const newUserGroups = [...groups, userGroupId]
                  setUsers(
                    oldUsers =>
                      oldUsers?.map(otherUser =>
                        otherUser.userId !== user.userId
                          ? otherUser
                          : object.merge(user, { userGroups: newUserGroups })
                      ) ?? null
                  )
                  await backend.changeUserGroup(
                    user.userId,
                    { userGroups: newUserGroups },
                    user.name
                  )
                } catch (error) {
                  toastAndLog('changeUserGroupsError', error)
                  setUsers(
                    oldUsers =>
                      oldUsers?.map(otherUser =>
                        otherUser.userId !== user.userId
                          ? otherUser
                          : object.merge(user, {
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

  React.useEffect(() => {
    void backend.listUsers().then(newUsers => {
      setUsers(newUsers)
    })
  }, [backend])

  const doDeleteUserGroup = async (userGroup: backendModule.UserGroupInfo) => {
    setUsers(
      oldUsers =>
        oldUsers?.map(user =>
          user.userGroups?.includes(userGroup.id) !== true
            ? user
            : object.merge(user, {
                userGroups: user.userGroups.filter(userGroupId => userGroupId !== userGroup.id),
              })
        ) ?? null
    )
    try {
      await backend.deleteUserGroup(userGroup.id, userGroup.groupName)
    } catch (error) {
      const usersInGroup = usersByGroup.get(userGroup.id)
      if (usersInGroup != null) {
        const userIds = new Set(usersInGroup.map(user => user.userId))
        setUsers(
          oldUsers =>
            oldUsers?.map(user =>
              !userIds.has(user.userId) || user.userGroups?.includes(userGroup.id) === true
                ? user
                : object.merge(user, {
                    userGroups: [...(user.userGroups ?? []), userGroup.id],
                  })
            ) ?? null
        )
      }
    }
  }

  const doRemoveUserFromUserGroup = async (
    user: backendModule.User,
    userGroup: backendModule.UserGroupInfo
  ) => {
    try {
      const intermediateUserGroups =
        user.userGroups?.filter(userGroupId => userGroupId !== userGroup.id) ?? null
      const newUserGroups = intermediateUserGroups?.length === 0 ? null : intermediateUserGroups
      setUsers(
        oldUsers =>
          oldUsers?.map(otherUser =>
            otherUser.userId !== user.userId
              ? otherUser
              : object.merge(user, { userGroups: newUserGroups })
          ) ?? null
      )
      await backend.changeUserGroup(user.userId, { userGroups: newUserGroups ?? [] }, user.name)
    } catch (error) {
      setUsers(
        oldUsers =>
          oldUsers?.map(otherUser =>
            otherUser.userId !== user.userId
              ? otherUser
              : object.merge(user, {
                  userGroups: [...(otherUser.userGroups ?? []), userGroup.id],
                })
          ) ?? null
      )
    }
  }

  return (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <SettingsSection noFocusArea title={getText('userGroups')}>
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                setModal(<NewUserGroupModal onSubmit={doRefresh} />)
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
              <aria.Column className="border-0" />
              <aria.Column
                isRowHeader
                className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
              >
                {getText('userGroup')}
              </aria.Column>
            </aria.TableHeader>
            <aria.TableBody className="select-text">
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
                userGroups.flatMap(userGroup => [
                  <aria.Row key={userGroup.id} id={userGroup.id} className="group h-row">
                    <aria.Cell className="flex bg-transparent p transparent group-hover-2:opacity-100">
                      <UnstyledButton
                        onPress={() => {
                          void doDeleteUserGroup(userGroup)
                        }}
                      >
                        <img src={Cross2} className="size-icon" />
                      </UnstyledButton>
                    </aria.Cell>
                    <aria.Cell className="text rounded-l-full border-x-2 border-transparent bg-clip-padding px-cell-x last:rounded-r-full last:border-r-0">
                      {userGroup.groupName}
                    </aria.Cell>
                  </aria.Row>,
                  (usersByGroup.get(userGroup.id) ?? []).map(user => (
                    <aria.Row key={user.userId} id={user.userId} className="h-row">
                      <aria.Cell className="bg-transparent p transparent group-hover-2:opacity-100">
                        <UnstyledButton
                          onPress={() => {
                            void doRemoveUserFromUserGroup(user, userGroup)
                          }}
                        >
                          <img src={Cross2} className="size-icon" />
                        </UnstyledButton>
                      </aria.Cell>
                      <aria.Cell className="text border-x-2 border-transparent bg-clip-padding rounded-rows-skip-level last:border-r-0">
                        <div className="ml-indent-1 flex h-row min-w-max items-center whitespace-nowrap rounded-full">
                          <aria.Text className="grow px-name-column-x py-name-column-y">
                            {user.name}
                          </aria.Text>
                        </div>
                      </aria.Cell>
                    </aria.Row>
                  )),
                ])
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
