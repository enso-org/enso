/** @file Settings tab for viewing and editing roles for all users in the organization. */
import { Cell, Column, Row, Table, TableBody, TableHeader } from '#/components/aria'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { ComboBox } from '#/components/Inputs/ComboBox'
import { Input } from '#/components/Inputs/Input'
import { Menu } from '#/components/Menu'
import { PaywallDialogButton } from '#/components/Paywall'
import { ProfilePicture } from '#/components/ProfilePicture'
import { Scroller } from '#/components/Scroller'
import { Text } from '#/components/Text'
import { UserWithPopover } from '#/components/UserWithPopover'
import { VisualTooltip } from '#/components/VisualTooltip'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { normalizeName } from '#/utilities/string'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useFullUserSession, useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import type { EmailAddress, User, UserGroupInfo } from 'enso-common/src/services/Backend'
import { useState } from 'react'

/** The maximum number of user icons per row. */
const MAXIMUM_USER_ICONS = 6

const USER_GROUP_SETTINGS_SECTION_STYLES = tv({
  base: '',
  slots: {
    tableContainer: 'min-h-0 flex-1',
    table: 'max-w-3xl table-fixed self-start rounded-rows',
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

/** Settings tab for viewing and editing organization teams. */
export function UserGroupsSettingsSection() {
  const [userGroup, setUserGroup] = useState<UserGroupInfo | null>(null)

  if (userGroup == null) {
    return <UserGroupsSettingsRootSection setUserGroup={setUserGroup} />
  } else {
    return (
      <UserGroupSettingsSection
        userGroup={userGroup}
        unsetUserGroup={() => {
          setUserGroup(null)
        }}
      />
    )
  }
}

/** Props for a {@link UserGroupsSettingsRootSection}. */
export interface UserGroupsSettingsRootSectionProps {
  readonly setUserGroup: (userGroup: UserGroupInfo) => void
}

/** Settings tab for viewing and editing organization teams. */
function UserGroupsSettingsRootSection(props: UserGroupsSettingsRootSectionProps) {
  const { setUserGroup } = props

  const { remoteBackend: backend } = useBackends()
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
    <div className="flex min-h-0 flex-1 flex-col gap-2">
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
      <Scroller
        scrollbar
        orientation="vertical"
        className={styles.tableContainer()}
        shadowStartClassName="mt-8"
      >
        <Table aria-label={getText('userGroups')} className={styles.table()}>
          <TableHeader className="sticky top-0 z-1 h-row bg-dashboard">
            <Column isRowHeader className={styles.column({ className: 'w-48 min-w-48' })}>
              {getText('userGroup')}
            </Column>
            <Column isRowHeader className={styles.column({ className: 'w-[21rem] min-w-[21rem]' })}>
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
              <Row className="h-10">
                <Cell
                  ref={(el) => {
                    if (!el) {
                      return
                    }
                    // This is SAFE; `react-aria-components` simply is missing types.
                    // This will be unnecessary when the `react-aria-components` dependency is updated as it adds support for `colSpan`.
                    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-magic-numbers
                    ;(el as HTMLTableCellElement).colSpan = 999
                  }}
                  className="px-2.5 placeholder"
                >
                  {isAdmin ?
                    getText('youHaveNoUserGroupsAdmin')
                  : getText('youHaveNoUserGroupsNonAdmin')}
                </Cell>
              </Row>
            : (userGroup) => (
                <UserGroupRow
                  key={userGroup.id}
                  userGroup={userGroup}
                  setUserGroup={setUserGroup}
                />
              )
            }
          </TableBody>
        </Table>
      </Scroller>
    </div>
  )
}

/** Props for a {@link UserGroupRow}. */
interface UserGroupRowProps {
  readonly userGroup: UserGroupInfo
  readonly setUserGroup: (userGroup: UserGroupInfo) => void
}

/** A row representing a user group. */
function UserGroupRow(props: UserGroupRowProps) {
  const { userGroup, setUserGroup } = props

  const { remoteBackend: backend } = useBackends()
  const { user } = useFullUserSession()
  const { getText } = useText()
  const isAdmin = user.isOrganizationAdmin

  const deleteUserGroupRaw = useMutationCallback(backendMutationOptions(backend, 'deleteUserGroup'))
  const deleteUserGroup = async () => {
    unsetModal()
    await deleteUserGroupRaw([userGroup.id, userGroup.groupName])
  }

  const { data: allUsers } = useSuspenseQuery(backendQueryOptions(backend, 'listUsers', []))
  const users = allUsers.filter((otherUser) =>
    (otherUser.userGroups ?? []).some((otherGroup) => otherGroup === userGroup.id),
  )

  const styles = USER_GROUP_ROW_STYLES()

  return (
    <Row id={userGroup.id} className="group h-10 select-none rounded-rows-child">
      <Cell className={styles.cell()}>
        <Text nowrap truncate="1" weight="semibold">
          {userGroup.groupName}
        </Text>
      </Cell>
      <Cell className={styles.cell()}>
        <div className="flex items-center gap-2">
          {users.slice(0, MAXIMUM_USER_ICONS).map((otherUser) => (
            <VisualTooltip
              key={otherUser.userId}
              tooltip={`${otherUser.name} (${otherUser.email})`}
              className="shrink-0"
            >
              <ProfilePicture picture={otherUser.profilePicture} name={otherUser.name} />
            </VisualTooltip>
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
        </div>
      </Cell>
      {isAdmin && (
        <Cell className={styles.cell()}>
          <Button.GroupJoin
            className="shrink-0 grow-0"
            buttonVariants={{ size: 'small', variant: 'outline' }}
          >
            <Button
              icon="people_settings"
              onPress={() => {
                setUserGroup(userGroup)
              }}
            >
              {getText('manageUsers')}
            </Button>
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

/** Props for a {@link UserGroupSettingsSection}. */
interface UserGroupSettingsSectionProps {
  readonly userGroup: UserGroupInfo
  readonly unsetUserGroup: () => void
}

/** A row representing a user group. */
function UserGroupSettingsSection(props: UserGroupSettingsSectionProps) {
  const { userGroup, unsetUserGroup } = props

  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()

  const deleteUserGroupRaw = useMutationCallback(backendMutationOptions(backend, 'deleteUserGroup'))
  const changeUserGroup = useMutationCallback(backendMutationOptions(backend, 'changeUserGroup'))
  const deleteUserGroup = async () => {
    unsetUserGroup()
    unsetModal()
    await deleteUserGroupRaw([userGroup.id, userGroup.groupName])
  }

  const { data: allUsers } = useSuspenseQuery(backendQueryOptions(backend, 'listUsers', []))
  const users = allUsers.filter((otherUser) =>
    (otherUser.userGroups ?? []).some((otherGroup) => otherGroup === userGroup.id),
  )

  const styles = USER_GROUP_SETTINGS_SECTION_STYLES()

  const removeUser = async (otherUser: User) => {
    const newUserGroups = (otherUser.groups ?? [])
      .filter((group) => group.id !== userGroup.id)
      .map((group) => group.id)
    await changeUserGroup([otherUser.userId, { userGroups: newUserGroups }, otherUser.name])
  }

  return (
    <div className="flex min-h-0 flex-1 flex-col gap-2">
      <Button
        variant="icon"
        size="medium"
        icon="arrow_circle_left"
        className="-ml-2"
        onPress={unsetUserGroup}
      >
        {getText('returnToGroupsList')}
      </Button>
      <Button.Group verticalAlign="center" className="flex-initial">
        <Popover.Trigger>
          <Button variant="outline">{getText('addUsers')}</Button>
          <Popover>
            <UserGroupAddUserForm userGroup={userGroup} />
          </Popover>
        </Popover.Trigger>
        <Button
          variant="delete-outline"
          onPress={() => {
            setModal(
              <ConfirmDeleteModal
                actionText={getText('deleteUserGroupActionText', userGroup.groupName)}
                onConfirm={deleteUserGroup}
              />,
            )
          }}
        >
          {getText('deleteUserGroup')}
        </Button>
      </Button.Group>
      <Text.Heading variant="subtitle">
        {getText('managingUserGroupX', userGroup.groupName)}
      </Text.Heading>
      <Scroller
        scrollbar
        orientation="vertical"
        className={styles.tableContainer()}
        shadowStartClassName="mt-8"
      >
        <Table>
          <TableHeader className="sticky top-0 z-1 h-row bg-dashboard">
            <Column isRowHeader className={styles.column({ className: 'w-80 max-w-80' })}>
              {getText('user')}
            </Column>
            <Column isRowHeader className={styles.column({ className: 'w-32 max-w-32' })}>
              {getText('actions')}
            </Column>
          </TableHeader>
          <TableBody>
            {users.map((otherUser) => (
              <Row key={otherUser.userId} className="group h-row rounded-rows-child">
                <Cell className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <Text truncate="1" className="block">
                    {otherUser.email}
                  </Text>
                  <Text truncate="1" className="block text-2xs text-primary/40">
                    {otherUser.name}
                  </Text>
                </Cell>
                <Cell className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <Button.GroupJoin
                    className="shrink-0 grow-0"
                    buttonVariants={{ size: 'small', variant: 'outline' }}
                  >
                    <Button
                      icon="data_output"
                      onPress={() => {
                        setModal(
                          <ConfirmDeleteModal
                            actionText={getText(
                              'removeUserFromUserGroupActionText',
                              otherUser.name,
                              userGroup.groupName,
                            )}
                            onConfirm={async () => {
                              unsetModal()
                              await removeUser(otherUser)
                            }}
                          />,
                        )
                      }}
                    >
                      {getText('remove')}
                    </Button>
                  </Button.GroupJoin>
                </Cell>
              </Row>
            ))}
            {users.length === 0 && (
              <Row>
                <Cell>
                  <Text color="muted">{getText('noUsersInThisGroup')}</Text>
                </Cell>
              </Row>
            )}
          </TableBody>
        </Table>
      </Scroller>
    </div>
  )
}

/** Props for {@link UserGroupAddUserForm}. */
interface UserGroupAddUserFormProps {
  readonly userGroup: UserGroupInfo
}

/** A form to add users to a user group. */
function UserGroupAddUserForm(props: UserGroupAddUserFormProps) {
  const { userGroup } = props

  const { getText } = useText()
  const { remoteBackend: backend } = useBackends()
  const changeUserGroup = useMutationCallback(backendMutationOptions(backend, 'changeUserGroup'))

  const { data: allUsers } = useSuspenseQuery(backendQueryOptions(backend, 'listUsers', []))
  const users = allUsers.filter((otherUser) => (otherUser.userGroups ?? []).includes(userGroup.id))
  const otherEmails = allUsers
    .filter((otherUser) => !(otherUser.userGroups ?? []).includes(userGroup.id))
    .map((user) => user.email)
  const usersByEmail = new Map(allUsers.map((user) => [user.email, user]))
  const emails = users.map((user) => user.email)

  const addUser = async (otherUser: Omit<User, 'groups'>) => {
    const newUserGroups = [...(otherUser.userGroups ?? []), userGroup.id]
    await changeUserGroup([otherUser.userId, { userGroups: newUserGroups }, otherUser.name])
  }

  return (
    <Form
      schema={(z) =>
        z.object({
          email: z
            .custom<EmailAddress>((s) => typeof s === 'string')
            .refine((s) => !emails.includes(s), getText('thisUserIsAlreadyInTheUserGroup')),
        })
      }
      defaultValues={{ email: '' }}
      onSubmit={async ({ email }) => {
        const user = usersByEmail.get(email)

        if (user == null) {
          return
        }

        await addUser(user)
      }}
    >
      {(form) => (
        <>
          <ComboBox
            form={form}
            name="email"
            aria-label={getText('user')}
            items={otherEmails}
            toTextValue={(email) => {
              const name = usersByEmail.get(email)?.name
              if (name == null) {
                return email
              }
              return `${name} (${email})`
            }}
          >
            {(email) => {
              const user = usersByEmail.get(email)

              return (
                user && (
                  <UserWithPopover
                    user={{ ...user, name: `${user.name} (${user.email})` }}
                    className="pointer-events-none"
                  />
                )
              )
            }}
          </ComboBox>
          <Button.Group>
            <Form.Submit>{getText('addUser')}</Form.Submit>
            <Dialog.Close variant="outline">{getText('done')}</Dialog.Close>
          </Button.Group>
        </>
      )}
    </Form>
  )
}

/** A form to create a user group. */
function NewUserGroupForm() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: userGroups } = useSuspenseQuery(backendQueryOptions(backend, 'listUserGroups', []))
  const userGroupNames = new Set(userGroups.map((group) => normalizeName(group.groupName)))
  const createUserGroup = useMutationCallback(backendMutationOptions(backend, 'createUserGroup'))

  return (
    <Form
      schema={(z) =>
        z.object({
          name: z
            .string()
            .min(1)
            .refine(
              (name) => !userGroupNames.has(normalizeName(name)),
              getText('duplicateUserGroupError'),
            ),
        })
      }
      method="dialog"
      onSubmit={({ name }) => createUserGroup([{ name }])}
    >
      <Text.Heading variant="subtitle">{getText('newUserGroup')}</Text.Heading>
      <Input name="name" label={getText('name')} />
      <Button.Group className="relative">
        <Form.Submit />
        <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
      </Button.Group>
      <Form.FormError />
    </Form>
  )
}
