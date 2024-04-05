/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import * as toast from 'react-toastify'
import isEmail from 'validator/es/lib/isEmail'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Autocomplete from '#/components/Autocomplete'
import Permission from '#/components/dashboard/Permission'
import PermissionSelector from '#/components/dashboard/PermissionSelector'
import Modal from '#/components/Modal'

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
  readonly item: Asset
  readonly setItem: React.Dispatch<React.SetStateAction<Asset>>
  readonly self: backendModule.UserPermission
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
  const { user: user } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const [permissions, setPermissions] = React.useState(item.permissions ?? [])
  // FIXME: setUsersAndUserGroups
  const [usersAndUserGroups, setUsers] = React.useState<
    readonly (backendModule.UserGroupInfo | backendModule.UserInfo)[]
  >([])
  const [email, setEmail] = React.useState<string | null>(null)
  const [action, setAction] = React.useState(permissionsModule.PermissionAction.view)
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const editablePermissions = React.useMemo(
    () =>
      self.permission === permissionsModule.PermissionAction.own
        ? permissions
        : permissions.filter(
            permission => permission.permission !== permissionsModule.PermissionAction.own
          ),
    [permissions, self.permission]
  )
  const permissionsHoldersNames = React.useMemo(
    () => new Set(item.permissions?.map(backendModule.getAssetPermissionName)),
    [item.permissions]
  )
  const emailsOfUsersWithPermission = React.useMemo(
    () =>
      new Set<string>(
        item.permissions?.flatMap(userPermission =>
          'user' in userPermission ? [userPermission.user.email] : []
        )
      ),
    [item.permissions]
  )
  const isOnlyOwner = React.useMemo(
    () =>
      self.permission === permissionsModule.PermissionAction.own &&
      permissions.every(
        permission =>
          permission.permission !== permissionsModule.PermissionAction.own ||
          (backendModule.isUserPermission(permission) && permission.user.userId === user?.userId)
      ),
    [user?.userId, permissions, self.permission]
  )

  React.useEffect(() => {
    // This is SAFE, as the type of asset is not being changed.
    // eslint-disable-next-line no-restricted-syntax
    setItem(object.merger({ permissions } as Partial<Asset>))
  }, [permissions, /* should never change */ setItem])

  if (backend.type === backendModule.BackendType.local || user == null) {
    // This should never happen - the local backend does not have the "shared with" column,
    // and `organization` is absent only when offline - in which case the user should only
    // be able to access the local backend.
    // This MUST be an error, otherwise the hooks below are considered as conditionally called.
    throw new Error('Cannot share assets on the local backend.')
  } else {
    const listedUsers = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [])
    const listedUserGroups = asyncEffectHooks.useAsyncEffect(
      null,
      () => backend.listUserGroups(),
      []
    )
    const canAdd = React.useMemo(
      () => [
        ...(listedUsers ?? []).filter(
          listedUser =>
            !permissionsHoldersNames.has(listedUser.name) &&
            !emailsOfUsersWithPermission.has(listedUser.email)
        ),
        ...(listedUserGroups ?? []).filter(
          userGroup => !permissionsHoldersNames.has(userGroup.groupName)
        ),
      ],
      [emailsOfUsersWithPermission, permissionsHoldersNames, listedUsers, listedUserGroups]
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
            userOrGroup =>
              ('name' in userOrGroup && userOrGroup.name.toLowerCase() === lowercase) ||
              ('email' in userOrGroup && userOrGroup.email.toLowerCase() === lowercase) ||
              ('groupName' in userOrGroup && userOrGroup.groupName.toLowerCase() === lowercase)
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
          setUsers([])
          setEmail('')
          if (email != null) {
            await backend.inviteUser({
              organizationId: user.organizationId,
              userEmail: backendModule.EmailAddress(email),
            })
            toast.toast.success(getText('inviteSuccess', email))
          }
        } catch (error) {
          toastAndLog('couldNotInviteUser', error, email ?? '(unknown)')
        }
      } else {
        setUsers([])
        const addedPermissions = usersAndUserGroups.map<backendModule.AssetPermission>(
          newUserOrUserGroup =>
            'userId' in newUserOrUserGroup
              ? { user: newUserOrUserGroup, permission: action }
              : { userGroup: newUserOrUserGroup, permission: action }
        )
        const addedUsersIds = new Set(
          addedPermissions.flatMap(permission =>
            backendModule.isUserPermission(permission) ? [permission.user.userId] : []
          )
        )
        const addedUserGroupsIds = new Set(
          addedPermissions.flatMap(permission =>
            backendModule.isUserGroupPermission(permission) ? [permission.userGroup.id] : []
          )
        )
        const isPermissionNotBeingOverwritten = (permission: backendModule.AssetPermission) =>
          backendModule.isUserPermission(permission)
            ? !addedUsersIds.has(permission.user.userId)
            : !addedUserGroupsIds.has(permission.userGroup.id)

        try {
          setPermissions(oldPermissions =>
            [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...addedPermissions].sort(
              backendModule.compareAssetPermissions
            )
          )
          await backend.createPermission({
            actorsIds: addedPermissions.map(permission =>
              backendModule.isUserPermission(permission)
                ? permission.user.userId
                : permission.userGroup.id
            ),
            resourceId: item.id,
            action: action,
          })
        } catch (error) {
          setPermissions(oldPermissions =>
            [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...oldPermissions].sort(
              backendModule.compareAssetPermissions
            )
          )
          toastAndLog('setPermissionsError', error)
        }
      }
    }

    const doDelete = async (permissionId: backendModule.UserPermissionIdentifier) => {
      if (permissionId === self.user.userId) {
        doRemoveSelf()
      } else {
        const oldPermission = permissions.find(
          permission => backendModule.getAssetPermissionId(permission) === permissionId
        )
        try {
          setPermissions(oldPermissions =>
            oldPermissions.filter(
              permission => backendModule.getAssetPermissionId(permission) !== permissionId
            )
          )
          await backend.createPermission({
            actorsIds: [permissionId],
            resourceId: item.id,
            action: null,
          })
        } catch (error) {
          if (oldPermission != null) {
            setPermissions(oldPermissions =>
              [...oldPermissions, oldPermission].sort(backendModule.compareAssetPermissions)
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
            position != null
              ? {
                  left: position.left + window.scrollX,
                  top: position.top + window.scrollY,
                }
              : {}
          }
          className="sticky w-manage-permissions-modal rounded-default before:absolute before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
          onClick={mouseEvent => {
            mouseEvent.stopPropagation()
          }}
          onContextMenu={mouseEvent => {
            mouseEvent.stopPropagation()
            mouseEvent.preventDefault()
          }}
          onKeyDown={event => {
            if (event.key !== 'Escape') {
              event.stopPropagation()
            }
          }}
        >
          <div className="relative flex flex-col gap-modal rounded-default p-modal">
            <div className="flex h-row items-center gap-modal-tabs px-modal-tab-bar-x">
              <h2 className="text text-sm font-bold">{getText('invite')}</h2>
              {/* Space reserved for other tabs. */}
            </div>
            <form
              className="flex gap-input-with-button"
              onSubmit={event => {
                event.preventDefault()
                void doSubmit()
              }}
            >
              <div className="flex grow items-center gap-user-permission rounded-full border border-primary/10 px-manage-permissions-modal-input">
                <PermissionSelector
                  input
                  disabled={willInviteNewUser}
                  selfPermission={self.permission}
                  typeSelectorYOffsetPx={TYPE_SELECTOR_Y_OFFSET_PX}
                  action={permissionsModule.PermissionAction.view}
                  assetType={item.type}
                  onChange={setAction}
                />
                <div className="-mx-button-px grow">
                  <Autocomplete
                    multiple
                    autoFocus
                    placeholder={
                      // `listedUsers` will always include the current user.
                      listedUsers?.length !== 1
                        ? getText('inviteUserPlaceholder')
                        : getText('inviteFirstUserPlaceholder')
                    }
                    type="text"
                    itemsToString={items =>
                      items.length === 1 && items[0] != null && 'email' in items[0]
                        ? items[0].email
                        : getText('xUsersSelected', items.length)
                    }
                    values={usersAndUserGroups}
                    setValues={setUsers}
                    items={canAdd}
                    itemToKey={userOrGroup =>
                      'userId' in userOrGroup ? userOrGroup.userId : userOrGroup.id
                    }
                    itemToString={userOrGroup =>
                      'name' in userOrGroup
                        ? `${userOrGroup.name} (${userOrGroup.email})`
                        : userOrGroup.groupName
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
                  />
                </div>
              </div>
              <button
                type="submit"
                disabled={
                  willInviteNewUser
                    ? email == null || !isEmail(email)
                    : usersAndUserGroups.length === 0 ||
                      (email != null && emailsOfUsersWithPermission.has(email))
                }
                className="button bg-invite px-button-x text-tag-text selectable enabled:active"
              >
                <div className="h-text py-modal-invite-button-text-y">
                  {willInviteNewUser ? 'Invite' : 'Share'}
                </div>
              </button>
            </form>
            <div className="max-h-manage-permissions-modal-permissions-list overflow-auto px-manage-permissions-modal-input">
              {editablePermissions.map(permission => (
                <div
                  key={backendModule.getAssetPermissionName(permission)}
                  className="flex h-row items-center"
                >
                  <Permission
                    asset={item}
                    self={self}
                    isOnlyOwner={isOnlyOwner}
                    permission={permission}
                    setPermission={newPermission => {
                      const permissionId = backendModule.getAssetPermissionId(newPermission)
                      setPermissions(oldPermissions =>
                        oldPermissions.map(oldPermission =>
                          backendModule.getAssetPermissionId(oldPermission) === permissionId
                            ? newPermission
                            : oldPermission
                        )
                      )
                      if (permissionId === self.user.userId) {
                        // This must run only after the permissions have
                        // been updated through `setItem`.
                        setTimeout(() => {
                          unsetModal()
                        }, 0)
                      }
                    }}
                    doDelete={id => {
                      if (id === self.user.userId) {
                        unsetModal()
                      }
                      void doDelete(id)
                    }}
                  />
                </div>
              ))}
            </div>
          </div>
        </div>
      </Modal>
    )
  }
}
