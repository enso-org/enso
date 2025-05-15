/** @file Settings tab for viewing and editing roles for all users in the organization. */

import { useSuspenseQuery } from '@tanstack/react-query'

import { Cell, Column, Row, Table, TableBody, TableHeader } from '#/components/aria'
import { Button, Menu, Popover, Text } from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { PaywallDialogButton } from '#/components/Paywall'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useContextMenuRef } from '#/hooks/contextMenuHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { NewUserGroupForm } from '#/modals/NewUserGroupModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { setModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { type User, type UserGroupInfo } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'

/** The maximum number of user icons per row. */
const MAXIMUM_USER_ICONS = 6

const USER_GROUP_SETTINGS_SECTION_STYLES = tv({
  base: '',
  slots: {
    column:
      'w-full border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0',
  },
})

const USER_GROUP_ROW_STYLES = tv({
  base: '',
  slots: {
    cell: 'rounded-r-full border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:border-r-0',
  },
})

/** Settings tab for viewing and editing organization members. */
export default function UserGroupsSettingsSection() {
  const backend = useRemoteBackend()
  const { getText } = useText()
  const { user } = useFullUserSession()
  const { data: userGroups } = useSuspenseQuery(backendQueryOptions(backend, 'listUserGroups', []))
  const isAdmin = user.isOrganizationAdmin

  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })

  const isUnderPaywall = isFeatureUnderPaywall('userGroupsFull')
  const userGroupsLeft = isUnderPaywall ? 1 - userGroups.length : Infinity
  const shouldDisplayPaywall = isUnderPaywall ? userGroupsLeft <= 0 : false

  const styles = USER_GROUP_SETTINGS_SECTION_STYLES()

  return (
    <>
      {isAdmin && (
        <Button.Group verticalAlign="center" className="flex-initial">
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
            <Popover.Trigger>
              <Button variant="outline">{getText('newUserGroup')}</Button>
              <Popover size="small" placement="bottom left">
                <NewUserGroupForm />
              </Popover>
            </Popover.Trigger>
          )}

          {isUnderPaywall && (
            <span className="text-xs">
              {userGroupsLeft <= 0 ?
                getText('userGroupsPaywallMessage')
              : getText('userGroupsLimitMessage', userGroupsLeft)}
            </span>
          )}
        </Button.Group>
      )}
      <div className="min-h-0 flex-initial overflow-y-auto overflow-x-hidden transition-all lg:mb-2">
        <Table
          aria-label={getText('userGroups')}
          className="w-full max-w-3xl table-fixed self-start rounded-rows"
        >
          <TableHeader className="sticky top-0 z-1 h-row bg-dashboard">
            <Column isRowHeader className={styles.column()}>
              {getText('userGroup')}
            </Column>
            <Column isRowHeader className={styles.column()}>
              {getText('users')}
            </Column>
            {isAdmin && (
              <Column isRowHeader className={styles.column()}>
                {getText('actions')}
              </Column>
            )}
          </TableHeader>
          <TableBody items={userGroups} dependencies={[userGroups]} className="select-text">
            {userGroups.length === 0 ?
              <Row className="h-row">
                <Cell className="col-span-2 px-2.5 placeholder">
                  {isAdmin ?
                    getText('youHaveNoUserGroupsAdmin')
                  : getText('youHaveNoUserGroupsNonAdmin')}
                </Cell>
              </Row>
            : (userGroup) => <UserGroupRow userGroup={userGroup} />}
          </TableBody>
        </Table>
      </div>
    </>
  )
}

/** Props for a {@link UserGroupRow}. */
interface UserGroupRowProps {
  readonly userGroup: UserGroupInfo
}

/** A row representing a user group. */
function UserGroupRow(props: UserGroupRowProps) {
  const { userGroup } = props

  const backend = useRemoteBackend()
  const { user } = useFullUserSession()
  const { getText } = useText()
  const isAdmin = user.isOrganizationAdmin

  const deleteUserGroupRaw = useMutationCallback(backendMutationOptions(backend, 'deleteUserGroup'))
  const changeUserGroup = useMutationCallback(backendMutationOptions(backend, 'changeUserGroup'))
  const deleteUserGroup = () => deleteUserGroupRaw([userGroup.id, userGroup.groupName])

  const { data: allUsers } = useSuspenseQuery(backendQueryOptions(backend, 'listUsers', []))
  const users = allUsers.filter((otherUser) =>
    (otherUser.groups ?? []).some((otherGroup) => otherGroup.id === userGroup.id),
  )

  const styles = USER_GROUP_ROW_STYLES()

  const contextMenuRef = useContextMenuRef(
    getText('userGroupContextMenuLabel'),
    () => (
      <ContextMenuEntry
        action="delete"
        doAction={() => {
          setModal(
            <ConfirmDeleteModal
              defaultOpen
              actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
              onConfirm={deleteUserGroup}
            />,
          )
        }}
      />
    ),
    { enabled: isAdmin },
  )

  const removeUser = async (otherUser: User) => {
    const newUserGroups = (otherUser.groups ?? [])
      .filter((group) => group.id !== userGroup.id)
      .map((group) => group.id)
    await changeUserGroup([otherUser.userId, { userGroups: newUserGroups }, otherUser.name])
  }

  const addUser = async (otherUser: User) => {
    const newUserGroups = [...(otherUser.groups?.map((group) => group.id) ?? []), userGroup.id]
    await changeUserGroup([otherUser.userId, { userGroups: newUserGroups }, otherUser.name])
  }

  return (
    <Row
      id={userGroup.id}
      className="group h-row select-none rounded-rows-child"
      ref={contextMenuRef}
    >
      <Cell className={styles.cell()}>
        <Text nowrap truncate="1" weight="semibold">
          {userGroup.groupName}
        </Text>
      </Cell>
      <Cell className={styles.cell()}>
        {users.slice(0, MAXIMUM_USER_ICONS).map((otherUser) => (
          <Text nowrap truncate="1" weight="semibold">
            {otherUser.name}
          </Text>
        ))}
        {users.length === 0 && (
          <Text nowrap truncate="1">
            {getText('zeroUsers')}
          </Text>
        )}
        {users.length > MAXIMUM_USER_ICONS && (
          <Text nowrap truncate="1">
            {getText('plusXUsers', users.length - MAXIMUM_USER_ICONS)}
          </Text>
        )}
      </Cell>
      {isAdmin && (
        <Cell className={styles.cell()}>
          <Button.GroupJoin
            className="shrink-0 grow-0"
            buttonVariants={{ size: 'small', variant: 'outline' }}
          >
            <Button icon="people_settings">{getText('manageUsers')}</Button>
            <Menu.Trigger>
              <Button icon="folder_opened" iconPosition="end" variant="outline" />
              <Menu>
                <Menu.Item
                  icon="trash"
                  onAction={() => {
                    setModal(
                      <ConfirmDeleteModal
                        actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
                        onConfirm={deleteUserGroup}
                      />,
                    )
                  }}
                >
                  {getText('delete')}
                </Menu.Item>
              </Menu>
            </Menu.Trigger>
          </Button.GroupJoin>
        </Cell>
      )}
    </Row>
  )
}
