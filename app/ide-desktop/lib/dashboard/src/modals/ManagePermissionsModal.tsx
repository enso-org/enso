/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import * as toast from 'react-toastify'
import isEmail from 'validator/es/lib/isEmail'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import Autocomplete from '#/components/Autocomplete'
import PermissionSelector from '#/components/dashboard/PermissionSelector'
import Permissions from '#/components/dashboard/UserPermissions'
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
  const [permissions, setPermissions] = React.useState(item.permissions ?? [])
  const [usersAndUserGroups, setUsers] = React.useState<backendModule.SimpleUser[]>([])
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
  const usernamesOfUsersWithPermission = React.useMemo(
    () => new Set(item.permissions?.map(backendModule.getAssetPermissionName)),
    [item.permissions]
  )
  const emailsOfUsersWithPermission = React.useMemo(
    () =>
      new Set<string>(
        item.permissions?.flatMap(userPermission =>
          backendModule.isUserPermission(userPermission) ? [userPermission.user.user_email] : []
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
          (backendModule.isUserPermission(permission) && permission.user.user_email === user?.email)
      ),
    [user?.email, permissions, self.permission]
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
    const allUsers = React.useMemo(
      () =>
        (listedUsers ?? []).filter(
          listedUser =>
            !usernamesOfUsersWithPermission.has(listedUser.name) &&
            !emailsOfUsersWithPermission.has(listedUser.email)
        ),
      [emailsOfUsersWithPermission, usernamesOfUsersWithPermission, listedUsers]
    )
    const willInviteNewUser = React.useMemo(() => {
      if (usersAndUserGroups.length !== 0 || email == null || email === '') {
        return false
      } else {
        const lowercase = email.toLowerCase()
        return (
          lowercase !== '' &&
          !usernamesOfUsersWithPermission.has(lowercase) &&
          !emailsOfUsersWithPermission.has(lowercase) &&
          !allUsers.some(
            innerUser =>
              innerUser.name.toLowerCase() === lowercase ||
              innerUser.email.toLowerCase() === lowercase
          )
        )
      }
    }, [
      usersAndUserGroups.length,
      email,
      emailsOfUsersWithPermission,
      usernamesOfUsersWithPermission,
      allUsers,
    ])

    const doSubmit = async () => {
      if (willInviteNewUser) {
        try {
          setUsers([])
          setEmail('')
          if (email != null) {
            await backend.inviteUser({
              organizationId: user.id,
              userEmail: backendModule.EmailAddress(email),
            })
            toast.toast.success(`You've invited '${email}' to join Enso!`)
          }
        } catch (error) {
          toastAndLog(`Could not invite user '${email}'`, error)
        }
      } else {
        setUsers([])
        const addedPermissions = usersAndUserGroups.map<backendModule.AssetPermission>(
          newUserOrUserGroup => ({
            user: {
              // The names come from a third-party API and cannot be
              // changed.
              /* eslint-disable @typescript-eslint/naming-convention */
              organization_id: user.id,
              pk: newUserOrUserGroup.id,
              user_email: newUserOrUserGroup.email,
              user_name: newUserOrUserGroup.name,
              /* eslint-enable @typescript-eslint/naming-convention */
            },
            permission: action,
          })
        )
        const addedUsersIds = new Set(
          addedPermissions.flatMap(permission =>
            backendModule.isUserPermission(permission) ? [permission.user.pk] : []
          )
        )
        const addedUserGroupsIds = new Set(
          addedPermissions.flatMap(permission =>
            backendModule.isUserGroupPermission(permission) ? [permission.userGroup.sk] : []
          )
        )
        const oldUsersPermissions = permissions.filter(
          backendModule.isUserPermissionAnd(permission => addedUsersIds.has(permission.user.pk))
        )
        const isPermissionNotBeingOverwritten = (permission: backendModule.AssetPermission) =>
          backendModule.isUserPermission(permission)
            ? !addedUsersIds.has(permission.user.pk)
            : !addedUserGroupsIds.has(permission.userGroup.sk)

        try {
          setPermissions(oldPermissions =>
            [...oldPermissions.filter(isPermissionNotBeingOverwritten), ...addedPermissions].sort(
              backendModule.compareAssetPermissions
            )
          )
          await backend.createPermission({
            userSubjects: addedPermissions.map(permission =>
              backendModule.isUserPermission(permission)
                ? permission.user.pk
                : permission.userGroup.sk
            ),
            resourceId: item.id,
            action: action,
          })
        } catch (error) {
          setPermissions(oldPermissions =>
            [
              ...oldPermissions.filter(isPermissionNotBeingOverwritten),
              ...oldUsersPermissions,
            ].sort(backendModule.compareAssetPermissions)
          )
          const usernames = addedPermissions.map(backendModule.getAssetPermissionName)
          toastAndLog(`Could not set permissions for ${usernames.join(', ')}`, error)
        }
      }
    }

    const doDelete = async (permissionId: backendModule.UserPermissionIdentifier) => {
      if (permissionId === self.user.pk) {
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
            userSubjects: [permissionId],
            resourceId: item.id,
            action: null,
          })
        } catch (error) {
          if (oldPermission != null) {
            setPermissions(oldPermissions =>
              [...oldPermissions, oldPermission].sort(backendModule.compareAssetPermissions)
            )
          }
          toastAndLog(`Could not set permissions`, error)
        }
      }
    }

    return (
      <Modal
        centered={eventTarget == null}
        className="absolute overflow-hidden bg-dim w-full h-full top-0 left-0"
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
          className="sticky w-115.25 rounded-2xl before:absolute before:bg-frame-selected before:backdrop-blur-3xl before:rounded-2xl before:w-full before:h-full"
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
          <div className="relative flex flex-col rounded-2xl gap-2 p-2">
            <div>
              <h2 className="text-sm font-bold">Invite</h2>
              {/* Space reserved for other tabs. */}
            </div>
            <form
              className="flex gap-1"
              onSubmit={event => {
                event.preventDefault()
                void doSubmit()
              }}
            >
              <div className="flex items-center grow rounded-full border border-black/10 gap-2 px-1">
                <PermissionSelector
                  disabled={willInviteNewUser}
                  selfPermission={self.permission}
                  typeSelectorYOffsetPx={TYPE_SELECTOR_Y_OFFSET_PX}
                  action={permissionsModule.PermissionAction.view}
                  assetType={item.type}
                  onChange={setAction}
                />
                <Autocomplete
                  multiple
                  autoFocus
                  placeholder={
                    // `listedUsers` will always include the current user.
                    listedUsers?.length !== 1
                      ? 'Type usernames or emails to search or invite'
                      : 'Enter an email to invite someone'
                  }
                  type="text"
                  itemsToString={items =>
                    items.length === 1 && items[0] != null
                      ? items[0].email
                      : `${items.length} users selected`
                  }
                  values={usersAndUserGroups}
                  setValues={setUsers}
                  items={allUsers}
                  itemToKey={otherUser => otherUser.id}
                  itemToString={otherUser => `${otherUser.name} (${otherUser.email})`}
                  matches={(otherUser, text) =>
                    otherUser.email.toLowerCase().includes(text.toLowerCase()) ||
                    otherUser.name.toLowerCase().includes(text.toLowerCase())
                  }
                  text={email}
                  setText={setEmail}
                />
              </div>
              <button
                type="submit"
                disabled={
                  willInviteNewUser
                    ? email == null || !isEmail(email)
                    : usersAndUserGroups.length === 0 ||
                      (email != null && emailsOfUsersWithPermission.has(email))
                }
                className="text-tag-text bg-invite rounded-full px-2 py-1 disabled:opacity-30"
              >
                <div className="h-6 py-0.5">{willInviteNewUser ? 'Invite' : 'Share'}</div>
              </button>
            </form>
            <div className="overflow-auto pl-1 pr-12 max-h-80">
              {editablePermissions.map(permission => (
                <div
                  key={backendModule.getAssetPermissionName(permission)}
                  className="flex items-center h-8"
                >
                  <Permissions
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
                      if (permissionId === self.user.pk) {
                        setTimeout(unsetModal)
                      }
                    }}
                    doDelete={id => {
                      if (id === self.user.pk) {
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
