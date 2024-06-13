/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as mimeTypes from '#/data/mimeTypes'

import * as backendHooks from '#/hooks/backendHooks'
import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import UserGroupRow from '#/layouts/Settings/UserGroupRow'
import UserGroupUserRow from '#/layouts/Settings/UserGroupUserRow'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// =================================
// === UserGroupsSettingsSection ===
// =================================

/** Props for a {@link UserGroupsSettingsTab}. */
export interface UserGroupsSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function UserGroupsSettingsTab(props: UserGroupsSettingsTabProps) {
  const { backend } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const users = backendHooks.useBackendListUsers(backend)
  const userGroups = backendHooks.useBackendListUserGroupsWithUsers(backend)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const changeUserGroup = backendHooks.useBackendMutation(backend, 'changeUserGroup')
  const deleteUserGroup = backendHooks.useBackendMutation(backend, 'deleteUserGroup')
  const usersMap = React.useMemo(
    () => new Map((users ?? []).map(otherUser => [otherUser.userId, otherUser])),
    [users]
  )
  const isLoading = userGroups == null || users == null

  const { onScroll: onUserGroupsTableScroll, shadowClassName } =
    scrollHooks.useStickyTableHeaderOnScroll(rootRef, bodyRef, true)

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
                  await changeUserGroup.mutateAsync([
                    newUser.userId,
                    { userGroups: newUserGroups },
                    newUser.name,
                  ])
                } catch (error) {
                  toastAndLog('changeUserGroupsError', error)
                }
              }
            })
          }
        }
      }
    },
  })

  const doDeleteUserGroup = async (userGroup: backendModule.UserGroupInfo) => {
    try {
      await deleteUserGroup.mutateAsync([userGroup.id, userGroup.groupName])
    } catch (error) {
      toastAndLog('deleteUserGroupError', error, userGroup.groupName)
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
      await changeUserGroup.mutateAsync([
        otherUser.userId,
        { userGroups: newUserGroups ?? [] },
        otherUser.name,
      ])
    } catch (error) {
      toastAndLog('removeUserFromUserGroupError', error, otherUser.name, userGroup.groupName)
    }
  }

  return (
    <>
      <HorizontalMenuBar>
        <ariaComponents.Button
          size="custom"
          variant="custom"
          className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
          onPress={event => {
            const rect = event.target.getBoundingClientRect()
            const position = { pageX: rect.left, pageY: rect.top }
            setModal(<NewUserGroupModal backend={backend} event={position} />)
          }}
        >
          {getText('newUserGroup')}
        </ariaComponents.Button>
      </HorizontalMenuBar>
      <div
        ref={rootRef}
        className={tailwindMerge.twMerge(
          'overflow-auto overflow-x-hidden transition-all lg:mb-2',
          shadowClassName
        )}
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
            dependencies={[isLoading, userGroups]}
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
                  {userGroup.users.map(otherUser => (
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
    </>
  )
}
