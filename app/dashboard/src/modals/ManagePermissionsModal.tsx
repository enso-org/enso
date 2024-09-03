/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import { useMutation, useQuery } from '@tanstack/react-query'
import * as toast from 'react-toastify'
import isEmail from 'validator/es/lib/isEmail'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as billingHooks from '#/hooks/billing'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Autocomplete from '#/components/Autocomplete'
import Permission from '#/components/dashboard/Permission'
import PermissionSelector from '#/components/dashboard/PermissionSelector'
import Modal from '#/components/Modal'
import * as paywall from '#/components/Paywall'
import FocusArea from '#/components/styled/FocusArea'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'
import * as permissionsModule from '#/utilities/permissions'

// =================
// === Constants ===
// =================

/** The vertical offset of the `PermissionTypeSelector` from its parent element, for the
 * input to invite new users. */
const TYPE_SELECTOR_Y_OFFSET_PX = 32

// ==============================
// === ManagePermissionsModal ===
// ==============================

/** Props for a {@link ManagePermissionsModal}. */
export interface ManagePermissionsModalProps<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
> {
  readonly item: Pick<Asset, 'id' | 'permissions' | 'type'>
  readonly setItem: React.Dispatch<React.SetStateAction<Asset>>
  readonly self: backendModule.AssetPermission
  /** Remove the current user's permissions from this asset. This MUST be a prop because it should
   * change the assets list. */
  readonly doRemoveSelf: () => void
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManagePermissionsModal<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
>(props: ManagePermissionsModalProps<Asset>) {
  const { item, setItem, self, doRemoveSelf, eventTarget } = props
  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const { user } = authProvider.useFullUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()

  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
  const isUnderPaywall = isFeatureUnderPaywall('shareFull')

  const listedUsers = useQuery({
    queryKey: ['listUsers'],
    queryFn: () => remoteBackend.listUsers(),
    enabled: !isUnderPaywall,
    select: (data) => (isUnderPaywall ? [] : data),
  })

  const listedUserGroups = useQuery({
    queryKey: ['listUserGroups'],
    queryFn: () => remoteBackend.listUserGroups(),
  })

  const [permissions, setPermissions] = React.useState(item.permissions ?? [])
  const [usersAndUserGroups, setUserAndUserGroups] = React.useState<
    readonly (backendModule.UserGroupInfo | backendModule.UserInfo)[]
  >([])
  const [email, setEmail] = React.useState<string | null>(null)
  const [action, setAction] = React.useState(permissionsModule.PermissionAction.view)
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const editablePermissions = React.useMemo(
    () =>
      self.permission === permissionsModule.PermissionAction.own ?
        permissions
      : permissions.filter(
          (permission) => permission.permission !== permissionsModule.PermissionAction.own,
        ),
    [permissions, self.permission],
  )
  const permissionsHoldersNames = React.useMemo(
    () => new Set(item.permissions?.map(backendModule.getAssetPermissionName)),
    [item.permissions],
  )
  const emailsOfUsersWithPermission = React.useMemo(
    () =>
      new Set<string>(
        item.permissions?.flatMap((userPermission) =>
          'user' in userPermission ? [userPermission.user.email] : [],
        ),
      ),
    [item.permissions],
  )
  const isOnlyOwner = React.useMemo(
    () =>
      self.permission === permissionsModule.PermissionAction.own &&
      permissions.every(
        (permission) =>
          permission.permission !== permissionsModule.PermissionAction.own ||
          (backendModule.isUserPermission(permission) && permission.user.userId === user.userId),
      ),
    [user.userId, permissions, self.permission],
  )
  const selfId = backendModule.getAssetPermissionId(self)

  const inviteUserMutation = useMutation(backendMutationOptions(remoteBackend, 'inviteUser'))
  const createPermissionMutation = useMutation(
    backendMutationOptions(remoteBackend, 'createPermission'),
  )

  React.useEffect(() => {
    // This is SAFE, as the type of asset is not being changed.
    // eslint-disable-next-line no-restricted-syntax
    setItem(object.merger({ permissions } as Partial<Asset>))
  }, [permissions, setItem])

  const canAdd = React.useMemo(
    () => [
      ...(listedUsers.data ?? []).filter(
        (listedUser) =>
          !permissionsHoldersNames.has(listedUser.name) &&
          !emailsOfUsersWithPermission.has(listedUser.email),
      ),
      ...(listedUserGroups.data ?? []).filter(
        (userGroup) => !permissionsHoldersNames.has(userGroup.groupName),
      ),
    ],
    [emailsOfUsersWithPermission, permissionsHoldersNames, listedUsers, listedUserGroups],
  )
  const willInviteNewUser = React.useMemo(() => {
    if (usersAndUserGroups.length !== 0 || email == null || email === '') {
      return false
    } else {
      const lowercase = email.toLowerCase()
      return (
        lowercase !== '' &&
        !permissionsHoldersNames.has(lowercase) &&
        !emailsOfUsersWithPermission.has(lowercase) &&
        !canAdd.some(
          (userOrGroup) =>
            ('name' in userOrGroup && userOrGroup.name.toLowerCase() === lowercase) ||
            ('email' in userOrGroup && userOrGroup.email.toLowerCase() === lowercase) ||
            ('groupName' in userOrGroup && userOrGroup.groupName.toLowerCase() === lowercase),
        )
      )
    }
  }, [
    usersAndUserGroups.length,
    email,
    emailsOfUsersWithPermission,
    permissionsHoldersNames,
    canAdd,
  ])

  const doSubmit = async () => {
    if (willInviteNewUser) {
      try {
        setUserAndUserGroups([])
        setEmail('')
        if (email != null) {
          await inviteUserMutation.mutateAsync([{ userEmail: backendModule.EmailAddress(email) }])
          toast.toast.success(getText('inviteSuccess', email))
        }
      } catch (error) {
        toastAndLog('couldNotInviteUser', error, email ?? '(unknown)')
      }
    } else {
      setUserAndUserGroups([])
      const addedPermissions = usersAndUserGroups.map<backendModule.AssetPermission>(
        (newUserOrUserGroup) =>
          'userId' in newUserOrUserGroup ?
            { user: newUserOrUserGroup, permission: action }
          : { userGroup: newUserOrUserGroup, permission: action },
      )
      const addedUsersIds = new Set(
        addedPermissions.flatMap((permission) =>
          backendModule.isUserPermission(permission) ? [permission.user.userId] : [],
        ),
      )
      const addedUserGroupsIds = new Set(
        addedPermissions.flatMap((permission) =>
          backendModule.isUserGroupPermission(permission) ? [permission.userGroup.id] : [],
        ),
      )
      const isPermissionNotBeingOverwritten = (permission: backendModule.AssetPermission) =>
        backendModule.isUserPermission(permission) ?
          !addedUsersIds.has(permission.user.userId)
        : !addedUserGroupsIds.has(permission.userGroup.id)

      try {
        setPermissions((oldPermissions) =>
          [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...addedPermissions].sort(
            backendModule.compareAssetPermissions,
          ),
        )
        await createPermissionMutation.mutateAsync([
          {
            actorsIds: addedPermissions.map((permission) =>
              backendModule.isUserPermission(permission) ?
                permission.user.userId
              : permission.userGroup.id,
            ),
            resourceId: item.id,
            action: action,
          },
        ])
      } catch (error) {
        setPermissions((oldPermissions) =>
          [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...oldPermissions].sort(
            backendModule.compareAssetPermissions,
          ),
        )
        toastAndLog('setPermissionsError', error)
      }
    }
  }

  const doDelete = async (permissionId: backendModule.UserPermissionIdentifier) => {
    if (selfId === permissionId) {
      doRemoveSelf()
    } else {
      const oldPermission = permissions.find(
        (permission) => backendModule.getAssetPermissionId(permission) === permissionId,
      )
      try {
        setPermissions((oldPermissions) =>
          oldPermissions.filter(
            (permission) => backendModule.getAssetPermissionId(permission) !== permissionId,
          ),
        )
        await createPermissionMutation.mutateAsync([
          {
            actorsIds: [permissionId],
            resourceId: item.id,
            action: null,
          },
        ])
      } catch (error) {
        if (oldPermission != null) {
          setPermissions((oldPermissions) =>
            [...oldPermissions, oldPermission].sort(backendModule.compareAssetPermissions),
          )
        }
        toastAndLog('setPermissionsError', error)
      }
    }
  }

  return (
    <Modal
      centered={eventTarget == null}
      className="absolute left top size-full overflow-hidden bg-dim"
    >
      <div
        tabIndex={-1}
        style={
          position != null ?
            {
              left: position.left + window.scrollX,
              top: position.top + window.scrollY,
            }
          : {}
        }
        className="sticky w-manage-permissions-modal rounded-default before:absolute before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(mouseEvent) => {
          mouseEvent.stopPropagation()
        }}
        onContextMenu={(mouseEvent) => {
          mouseEvent.stopPropagation()
          mouseEvent.preventDefault()
        }}
      >
        <div className="relative flex flex-col gap-modal rounded-default p-modal">
          <div className="flex h-row items-center gap-modal-tabs px-modal-tab-bar-x">
            <aria.Heading level={2} className="text text-sm font-bold">
              {getText('invite')}
            </aria.Heading>
            {/* Space reserved for other tabs. */}
          </div>
          <FocusArea direction="horizontal">
            {(innerProps) => (
              <form
                className="flex gap-input-with-button"
                onSubmit={(event) => {
                  event.preventDefault()
                  void doSubmit()
                }}
                {...innerProps}
              >
                <div className="flex w-0 grow items-center gap-user-permission rounded-full border border-primary/10 px-1">
                  <PermissionSelector
                    isInput
                    isDisabled={willInviteNewUser}
                    selfPermission={self.permission}
                    typeSelectorYOffsetPx={TYPE_SELECTOR_Y_OFFSET_PX}
                    action={permissionsModule.PermissionAction.view}
                    assetType={item.type}
                    onChange={setAction}
                  />
                  <div className="grow">
                    <Autocomplete
                      multiple
                      autoFocus
                      placeholder={
                        // `listedUsers` will always include the current user.
                        (listedUsers.data ?? []).length > 1 ?
                          getText('inviteUserPlaceholder')
                        : getText('inviteFirstUserPlaceholder')
                      }
                      type="text"
                      itemsToString={(items) =>
                        items.length === 1 && items[0] != null ?
                          'email' in items[0] ?
                            items[0].email
                          : items[0].groupName
                        : getText('xUsersAndGroupsSelected', items.length)
                      }
                      values={usersAndUserGroups}
                      setValues={setUserAndUserGroups}
                      items={canAdd}
                      itemToKey={(userOrGroup) =>
                        'userId' in userOrGroup ? userOrGroup.userId : userOrGroup.id
                      }
                      matches={(userOrGroup, text) =>
                        ('email' in userOrGroup &&
                          userOrGroup.email.toLowerCase().includes(text.toLowerCase())) ||
                        ('name' in userOrGroup &&
                          userOrGroup.name.toLowerCase().includes(text.toLowerCase())) ||
                        ('groupName' in userOrGroup &&
                          userOrGroup.groupName.toLowerCase().includes(text.toLowerCase()))
                      }
                      text={email}
                      setText={setEmail}
                    >
                      {(userOrGroup) =>
                        'name' in userOrGroup ?
                          `${userOrGroup.name} (${userOrGroup.email})`
                        : userOrGroup.groupName
                      }
                    </Autocomplete>
                  </div>
                </div>
                <ariaComponents.Button
                  size="medium"
                  variant="submit"
                  isDisabled={
                    willInviteNewUser ?
                      email == null || !isEmail(email)
                    : usersAndUserGroups.length === 0 ||
                      (email != null && emailsOfUsersWithPermission.has(email))
                  }
                  onPress={doSubmit}
                >
                  {willInviteNewUser ? getText('invite') : getText('share')}
                </ariaComponents.Button>
              </form>
            )}
          </FocusArea>
          <div className="max-h-manage-permissions-modal-permissions-list overflow-auto px-manage-permissions-modal-input">
            {editablePermissions.map((permission) => (
              <div
                key={backendModule.getAssetPermissionName(permission)}
                className="flex h-row items-center"
              >
                <Permission
                  backend={remoteBackend}
                  asset={item}
                  self={self}
                  isOnlyOwner={isOnlyOwner}
                  permission={permission}
                  setPermission={(newPermission) => {
                    const permissionId = backendModule.getAssetPermissionId(newPermission)
                    setPermissions((oldPermissions) =>
                      oldPermissions.map((oldPermission) =>
                        backendModule.getAssetPermissionId(oldPermission) === permissionId ?
                          newPermission
                        : oldPermission,
                      ),
                    )
                    if (selfId === permissionId) {
                      // This must run only after the permissions have
                      // been updated through `setItem`.
                      setTimeout(() => {
                        unsetModal()
                      }, 0)
                    }
                  }}
                  doDelete={(id) => {
                    if (selfId === id) {
                      unsetModal()
                    }
                    void doDelete(id)
                  }}
                />
              </div>
            ))}
          </div>

          {isUnderPaywall && (
            <paywall.PaywallAlert feature="shareFull" label={getText('shareFullPaywallMessage')} />
          )}
        </div>
      </div>
    </Modal>
  )
}
