/** @file A modal with inputs for user email and permission level. */
import { useMemo, useState } from 'react'

import { useMutation, useQuery } from '@tanstack/react-query'
import { toast } from 'react-toastify'
import isEmail from 'validator/es/lib/isEmail'

import { Heading } from '#/components/aria'
import { Button } from '#/components/AriaComponents'
import Autocomplete from '#/components/Autocomplete'
import Permission from '#/components/dashboard/Permission'
import PermissionSelector from '#/components/dashboard/PermissionSelector'
import Modal from '#/components/Modal'
import { PaywallAlert } from '#/components/Paywall'
import FocusArea from '#/components/styled/FocusArea'
import { backendMutationOptions, useAssetPassiveListenerStrict } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import {
  compareAssetPermissions,
  EmailAddress,
  getAssetPermissionId,
  getAssetPermissionName,
  isUserGroupPermission,
  isUserPermission,
  type AnyAsset,
  type AssetPermission,
  type UserGroupInfo,
  type UserInfo,
  type UserPermissionIdentifier,
} from '#/services/Backend'
import { PermissionAction } from '#/utilities/permissions'

// =================
// === Constants ===
// =================

/**
 * The vertical offset of the `PermissionTypeSelector` from its parent element, for the
 * input to invite new users.
 */
const TYPE_SELECTOR_Y_OFFSET_PX = 32

// ==============================
// === ManagePermissionsModal ===
// ==============================

/** Props for a {@link ManagePermissionsModal}. */
export interface ManagePermissionsModalProps<Asset extends AnyAsset = AnyAsset> {
  readonly backend: Backend
  readonly category: Category
  readonly item: Pick<Asset, 'id' | 'parentId' | 'permissions' | 'type'>
  readonly self: AssetPermission
  /**
   * Remove the current user's permissions from this asset. This MUST be a prop because it should
   * change the assets list.
   */
  readonly doRemoveSelf: () => void
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/**
 * A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case.
 */
export default function ManagePermissionsModal<Asset extends AnyAsset = AnyAsset>(
  props: ManagePermissionsModalProps<Asset>,
) {
  const { backend, category, item: itemRaw, self, doRemoveSelf, eventTarget } = props
  const item = useAssetPassiveListenerStrict(backend.type, itemRaw.id, itemRaw.parentId, category)
  const remoteBackend = useRemoteBackend()
  const { user } = useFullUserSession()
  const { unsetModal } = useSetModal()
  const toastAndLog = useToastAndLog()
  const { getText } = useText()

  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
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

  const [permissions, setPermissions] = useState(item.permissions ?? [])
  const [usersAndUserGroups, setUserAndUserGroups] = useState<
    readonly (UserGroupInfo | UserInfo)[]
  >([])
  const [email, setEmail] = useState<string | null>(null)
  const [action, setAction] = useState(PermissionAction.view)
  const position = useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const editablePermissions = useMemo(
    () =>
      self.permission === PermissionAction.own ?
        permissions
      : permissions.filter((permission) => permission.permission !== PermissionAction.own),
    [permissions, self.permission],
  )
  const permissionsHoldersNames = useMemo(
    () => new Set(item.permissions?.map(getAssetPermissionName)),
    [item.permissions],
  )
  const emailsOfUsersWithPermission = useMemo(
    () =>
      new Set<string>(
        item.permissions?.flatMap((userPermission) =>
          'user' in userPermission ? [userPermission.user.email] : [],
        ),
      ),
    [item.permissions],
  )
  const isOnlyOwner = useMemo(
    () =>
      self.permission === PermissionAction.own &&
      permissions.every(
        (permission) =>
          permission.permission !== PermissionAction.own ||
          (isUserPermission(permission) && permission.user.userId === user.userId),
      ),
    [user.userId, permissions, self.permission],
  )
  const selfId = getAssetPermissionId(self)

  const inviteUserMutation = useMutation(backendMutationOptions(remoteBackend, 'inviteUser'))
  const createPermissionMutation = useMutation(
    backendMutationOptions(remoteBackend, 'createPermission'),
  )

  const canAdd = useMemo(
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
  const willInviteNewUser = useMemo(() => {
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
          await inviteUserMutation.mutateAsync([{ userEmail: EmailAddress(email) }])
          toast.success(getText('inviteSuccess', email))
        }
      } catch (error) {
        toastAndLog('couldNotInviteUser', error, email ?? '(unknown)')
      }
    } else {
      setUserAndUserGroups([])
      const addedPermissions = usersAndUserGroups.map<AssetPermission>((newUserOrUserGroup) =>
        'userId' in newUserOrUserGroup ?
          { user: newUserOrUserGroup, permission: action }
        : { userGroup: newUserOrUserGroup, permission: action },
      )
      const addedUsersIds = new Set(
        addedPermissions.flatMap((permission) =>
          isUserPermission(permission) ? [permission.user.userId] : [],
        ),
      )
      const addedUserGroupsIds = new Set(
        addedPermissions.flatMap((permission) =>
          isUserGroupPermission(permission) ? [permission.userGroup.id] : [],
        ),
      )
      const isPermissionNotBeingOverwritten = (permission: AssetPermission) =>
        isUserPermission(permission) ?
          !addedUsersIds.has(permission.user.userId)
        : !addedUserGroupsIds.has(permission.userGroup.id)

      try {
        setPermissions((oldPermissions) =>
          [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...addedPermissions].sort(
            compareAssetPermissions,
          ),
        )
        await createPermissionMutation.mutateAsync([
          {
            actorsIds: addedPermissions.map((permission) =>
              isUserPermission(permission) ? permission.user.userId : permission.userGroup.id,
            ),
            resourceId: item.id,
            action: action,
          },
        ])
      } catch (error) {
        setPermissions((oldPermissions) =>
          [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...oldPermissions].sort(
            compareAssetPermissions,
          ),
        )
        toastAndLog('setPermissionsError', error)
      }
    }
  }

  const doDelete = async (permissionId: UserPermissionIdentifier) => {
    if (selfId === permissionId) {
      doRemoveSelf()
    } else {
      const oldPermission = permissions.find(
        (permission) => getAssetPermissionId(permission) === permissionId,
      )
      try {
        setPermissions((oldPermissions) =>
          oldPermissions.filter((permission) => getAssetPermissionId(permission) !== permissionId),
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
            [...oldPermissions, oldPermission].sort(compareAssetPermissions),
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
            <Heading level={2} className="text text-sm font-bold">
              {getText('invite')}
            </Heading>
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
                    action={PermissionAction.view}
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
                <Button
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
                </Button>
              </form>
            )}
          </FocusArea>
          <div className="max-h-manage-permissions-modal-permissions-list overflow-auto px-manage-permissions-modal-input">
            {editablePermissions.map((permission) => (
              <div key={getAssetPermissionName(permission)} className="flex h-row items-center">
                <Permission
                  backend={remoteBackend}
                  asset={item}
                  self={self}
                  isOnlyOwner={isOnlyOwner}
                  permission={permission}
                  setPermission={(newPermission) => {
                    const permissionId = getAssetPermissionId(newPermission)
                    setPermissions((oldPermissions) =>
                      oldPermissions.map((oldPermission) =>
                        getAssetPermissionId(oldPermission) === permissionId ? newPermission : (
                          oldPermission
                        ),
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
            <PaywallAlert feature="shareFull" label={getText('shareFullPaywallMessage')} />
          )}
        </div>
      </div>
    </Modal>
  )
}
