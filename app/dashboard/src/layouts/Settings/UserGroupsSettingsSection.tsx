/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import { Cell, Column, Row, Table, TableBody, TableHeader, useDragAndDrop } from '#/components/aria'
import { Button, ButtonGroup } from '#/components/AriaComponents'
import { PaywallDialogButton } from '#/components/Paywall'
import StatelessSpinner, { SpinnerState } from '#/components/StatelessSpinner'
import { USER_MIME_TYPE } from '#/data/mimeTypes'
import {
  backendMutationOptions,
  useBackendQuery,
  useListUserGroupsWithUsers,
} from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useStickyTableHeaderOnScroll } from '#/hooks/scrollHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import NewUserGroupModal from '#/modals/NewUserGroupModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import {
  isPlaceholderUserGroupId,
  isUserGroupId,
  type User,
  type UserGroupInfo,
} from '#/services/Backend'
import { twMerge } from '#/utilities/tailwindMerge'
import UserGroupRow from './UserGroupRow'
import UserGroupUserRow from './UserGroupUserRow'

// =================================
// === UserGroupsSettingsSection ===
// =================================

/** Props for a {@link UserGroupsSettingsSection}. */
export interface UserGroupsSettingsSectionProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function UserGroupsSettingsSection(props: UserGroupsSettingsSectionProps) {
  const { backend } = props
  const { setModal } = useSetModal()
  const { getText } = useText()
  const { user } = useFullUserSession()
  const toastAndLog = useToastAndLog()
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const userGroups = useListUserGroupsWithUsers(backend)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const changeUserGroup = useMutation(
    backendMutationOptions(backend, 'changeUserGroup'),
  ).mutateAsync
  const deleteUserGroup = useMutation(
    backendMutationOptions(backend, 'deleteUserGroup'),
  ).mutateAsync
  const usersMap = React.useMemo(
    () => new Map((users ?? []).map((otherUser) => [otherUser.userId, otherUser])),
    [users],
  )
  const isLoading = userGroups == null || users == null
  const isAdmin = user.isOrganizationAdmin

  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })

  const isUnderPaywall = isFeatureUnderPaywall('userGroupsFull')
  const userGroupsLeft = isUnderPaywall ? 1 - (userGroups?.length ?? 0) : Infinity
  const shouldDisplayPaywall = isUnderPaywall ? userGroupsLeft <= 0 : false

  const { onScroll: onUserGroupsTableScroll, shadowClassName } = useStickyTableHeaderOnScroll(
    rootRef,
    bodyRef,
    { trackShadowClass: true },
  )

  const { dragAndDropHooks } = useDragAndDrop({
    isDisabled: !isAdmin,
    getDropOperation: (target, types, allowedOperations) =>
      (
        allowedOperations.includes('copy') &&
        types.has(USER_MIME_TYPE) &&
        target.type === 'item' &&
        typeof target.key === 'string' &&
        isUserGroupId(target.key) &&
        !isPlaceholderUserGroupId(target.key)
      ) ?
        'copy'
      : 'cancel',
    onItemDrop: (event) => {
      if (typeof event.target.key === 'string' && isUserGroupId(event.target.key)) {
        const userGroupId = event.target.key
        for (const item of event.items) {
          if (item.kind === 'text' && item.types.has(USER_MIME_TYPE)) {
            void item.getText(USER_MIME_TYPE).then(async (text) => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const newUser: User = JSON.parse(text)
              const groups = usersMap.get(newUser.userId)?.userGroups ?? []
              if (!groups.includes(userGroupId)) {
                try {
                  const newUserGroups = [...groups, userGroupId]
                  await changeUserGroup([
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

  const doDeleteUserGroup = async (userGroup: UserGroupInfo) => {
    try {
      await deleteUserGroup([userGroup.id, userGroup.groupName])
    } catch (error) {
      toastAndLog('deleteUserGroupError', error, userGroup.groupName)
    }
  }

  const doRemoveUserFromUserGroup = async (otherUser: User, userGroup: UserGroupInfo) => {
    try {
      const intermediateUserGroups =
        otherUser.userGroups?.filter((userGroupId) => userGroupId !== userGroup.id) ?? null
      const newUserGroups = intermediateUserGroups?.length === 0 ? null : intermediateUserGroups
      await changeUserGroup([otherUser.userId, { userGroups: newUserGroups ?? [] }, otherUser.name])
    } catch (error) {
      toastAndLog('removeUserFromUserGroupError', error, otherUser.name, userGroup.groupName)
    }
  }

  return (
    <>
      {isAdmin && (
        <ButtonGroup verticalAlign="center">
          {shouldDisplayPaywall && (
            <PaywallDialogButton
              feature="userGroupsFull"
              variant="outline"
              size="medium"
              rounded="full"
              iconPosition="end"
              tooltip={getText('userGroupsPaywallMessage')}
            >
              {getText('newUserGroup')}
            </PaywallDialogButton>
          )}
          {!shouldDisplayPaywall && (
            <Button
              size="medium"
              variant="outline"
              onPress={(event) => {
                const rect = event.target.getBoundingClientRect()
                const position = { pageX: rect.left, pageY: rect.top }
                setModal(<NewUserGroupModal backend={backend} event={position} />)
              }}
            >
              {getText('newUserGroup')}
            </Button>
          )}

          {isUnderPaywall && (
            <span className="text-xs">
              {userGroupsLeft <= 0 ?
                getText('userGroupsPaywallMessage')
              : getText('userGroupsLimitMessage', userGroupsLeft)}
            </span>
          )}
        </ButtonGroup>
      )}
      <div
        ref={rootRef}
        className={twMerge(
          'overflow-auto overflow-x-hidden transition-all lg:mb-2',
          shadowClassName,
        )}
        onScroll={onUserGroupsTableScroll}
      >
        <Table
          aria-label={getText('userGroups')}
          className="w-full max-w-3xl table-fixed self-start rounded-rows"
          dragAndDropHooks={dragAndDropHooks}
        >
          <TableHeader className="sticky top h-row">
            <Column
              isRowHeader
              className="w-full border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
            >
              {getText('userGroup')}
            </Column>
            {/* Delete button. */}
            <Column className="relative border-0" />
          </TableHeader>
          <TableBody
            ref={bodyRef}
            items={userGroups ?? []}
            dependencies={[isLoading, userGroups]}
            className="select-text"
          >
            {isLoading ?
              <Row className="h-row">
                <Cell
                  ref={(element) => {
                    if (element instanceof HTMLTableCellElement) {
                      element.colSpan = 2
                    }
                  }}
                >
                  <div className="flex justify-center">
                    <StatelessSpinner size={32} state={SpinnerState.loadingMedium} />
                  </div>
                </Cell>
              </Row>
            : userGroups.length === 0 ?
              <Row className="h-row">
                <Cell className="col-span-2 px-2.5 placeholder">
                  {isAdmin ?
                    getText('youHaveNoUserGroupsAdmin')
                  : getText('youHaveNoUserGroupsNonAdmin')}
                </Cell>
              </Row>
            : (userGroup) => (
                <>
                  <UserGroupRow userGroup={userGroup} doDeleteUserGroup={doDeleteUserGroup} />
                  {userGroup.users.map((otherUser) => (
                    <UserGroupUserRow
                      key={otherUser.userId}
                      user={otherUser}
                      userGroup={userGroup}
                      doRemoveUserFromUserGroup={doRemoveUserFromUserGroup}
                    />
                  ))}
                </>
              )
            }
          </TableBody>
        </Table>
      </div>
    </>
  )
}
