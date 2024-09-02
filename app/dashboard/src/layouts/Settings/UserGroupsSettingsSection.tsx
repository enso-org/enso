/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import * as mimeTypes from '#/data/mimeTypes'

import {
  backendMutationOptions,
  useListUserGroupsWithUsers,
  useListUsers,
} from '#/hooks/backendHooks'
import * as billingHooks from '#/hooks/billing'
import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import UserGroupRow from '#/layouts/Settings/UserGroupRow'
import UserGroupUserRow from '#/layouts/Settings/UserGroupUserRow'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as paywallComponents from '#/components/Paywall'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

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
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { user } = authProvider.useFullUserSession()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const users = useListUsers(backend)
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

  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })

  const isUnderPaywall = isFeatureUnderPaywall('userGroupsFull')
  const userGroupsLeft = isUnderPaywall ? 1 - (userGroups?.length ?? 0) : Infinity
  const shouldDisplayPaywall = isUnderPaywall ? userGroupsLeft <= 0 : false

  const { onScroll: onUserGroupsTableScroll, shadowClassName } =
    scrollHooks.useStickyTableHeaderOnScroll(rootRef, bodyRef, { trackShadowClass: true })

  const { dragAndDropHooks } = aria.useDragAndDrop({
    isDisabled: !isAdmin,
    getDropOperation: (target, types, allowedOperations) =>
      (
        allowedOperations.includes('copy') &&
        types.has(mimeTypes.USER_MIME_TYPE) &&
        target.type === 'item' &&
        typeof target.key === 'string' &&
        backendModule.isUserGroupId(target.key) &&
        !backendModule.isPlaceholderUserGroupId(target.key)
      ) ?
        'copy'
      : 'cancel',
    onItemDrop: (event) => {
      if (typeof event.target.key === 'string' && backendModule.isUserGroupId(event.target.key)) {
        const userGroupId = event.target.key
        for (const item of event.items) {
          if (item.kind === 'text' && item.types.has(mimeTypes.USER_MIME_TYPE)) {
            void item.getText(mimeTypes.USER_MIME_TYPE).then(async (text) => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const newUser: backendModule.User = JSON.parse(text)
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

  const doDeleteUserGroup = async (userGroup: backendModule.UserGroupInfo) => {
    try {
      await deleteUserGroup([userGroup.id, userGroup.groupName])
    } catch (error) {
      toastAndLog('deleteUserGroupError', error, userGroup.groupName)
    }
  }

  const doRemoveUserFromUserGroup = async (
    otherUser: backendModule.User,
    userGroup: backendModule.UserGroupInfo,
  ) => {
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
        <ariaComponents.ButtonGroup verticalAlign="center">
          {shouldDisplayPaywall && (
            <paywallComponents.PaywallDialogButton
              feature="userGroupsFull"
              variant="outline"
              size="medium"
              rounded="full"
              iconPosition="end"
              tooltip={getText('userGroupsPaywallMessage')}
            >
              {getText('newUserGroup')}
            </paywallComponents.PaywallDialogButton>
          )}
          {!shouldDisplayPaywall && (
            <ariaComponents.Button
              size="medium"
              variant="outline"
              onPress={(event) => {
                const rect = event.target.getBoundingClientRect()
                const position = { pageX: rect.left, pageY: rect.top }
                setModal(<NewUserGroupModal backend={backend} event={position} />)
              }}
            >
              {getText('newUserGroup')}
            </ariaComponents.Button>
          )}

          {isUnderPaywall && (
            <span className="text-xs">
              {userGroupsLeft <= 0 ?
                getText('userGroupsPaywallMessage')
              : getText('userGroupsLimitMessage', userGroupsLeft)}
            </span>
          )}
        </ariaComponents.ButtonGroup>
      )}
      <div
        ref={rootRef}
        className={tailwindMerge.twMerge(
          'overflow-auto overflow-x-hidden transition-all lg:mb-2',
          shadowClassName,
        )}
        onScroll={onUserGroupsTableScroll}
      >
        <aria.Table
          aria-label={getText('userGroups')}
          className="w-full max-w-3xl table-fixed self-start rounded-rows"
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
            {isLoading ?
              <aria.Row className="h-row">
                <aria.Cell
                  ref={(element) => {
                    if (element instanceof HTMLTableCellElement) {
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
            : userGroups.length === 0 ?
              <aria.Row className="h-row">
                <aria.Cell className="col-span-2 px-2.5 placeholder">
                  {isAdmin ?
                    getText('youHaveNoUserGroupsAdmin')
                  : getText('youHaveNoUserGroupsNonAdmin')}
                </aria.Cell>
              </aria.Row>
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
          </aria.TableBody>
        </aria.Table>
      </div>
    </>
  )
}
