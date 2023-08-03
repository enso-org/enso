/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'

import Autocomplete from './autocomplete'
import Modal from './modal'
import PermissionSelector from './permissionSelector'
import UserPermissions from './userPermissions'

// =================
// === Constants ===
// =================

/** The maximum number of items to show in the {@link Autocomplete} before it switches to showing
 * "X items selected". */
const MAX_AUTOCOMPLETE_ITEMS_TO_SHOW = 3

// ==============================
// === ManagePermissionsModal ===
// ==============================

/** Props for a {@link ManagePermissionsModal}. */
export interface ManagePermissionsModalProps {
    item: backendModule.AnyAsset
    setItem: React.Dispatch<React.SetStateAction<backendModule.AnyAsset>>
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManagePermissionsModal(props: ManagePermissionsModalProps) {
    const { item, setItem, eventTarget } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [permissions, setPermissions] = React.useState(item.permissions ?? [])
    const [usernamesOrEmails, setUsernamesOrEmails] = React.useState<string[]>([])
    const [action, setAction] = React.useState(backendModule.PermissionAction.view)
    const emailValidityRef = React.useRef<HTMLInputElement>(null)
    const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
    const usernamesOfUsersWithPermission = React.useMemo(
        () => new Set(item.permissions?.map(userPermission => userPermission.user.user_name)),
        [item.permissions]
    )
    const emailsOfUsersWithPermission = React.useMemo(
        () =>
            new Set<string>(
                item.permissions?.map(userPermission => userPermission.user.user_email)
            ),
        [item.permissions]
    )

    React.useEffect(() => {
        setItem(oldItem => ({ ...oldItem, permissions }))
    }, [permissions, /* should never change */ setItem])

    if (backend.type === backendModule.BackendType.local || organization == null) {
        // This should never happen - the local backend does not have the "shared with" column,
        // and `organization` is absent only when offline - in which case the user should only
        // be able to access the local backend.
        // This MUST be an error, otherwise the hooks below are considered as conditionally called.
        throw new Error('Unable to share projects on the local backend.')
    } else {
        const listedUsers = hooks.useAsyncEffect([], () => backend.listUsers(), [])
        const emailToUsername = React.useMemo(
            () => Object.fromEntries(listedUsers.map(user => [user.email, user.name])),
            [listedUsers]
        )
        const users = React.useMemo(
            () =>
                listedUsers.filter(
                    listedUser =>
                        !usernamesOfUsersWithPermission.has(listedUser.name) &&
                        !emailsOfUsersWithPermission.has(listedUser.email)
                ),
            [emailsOfUsersWithPermission, usernamesOfUsersWithPermission, listedUsers]
        )
        const matchingUsers = React.useMemo(() => {
            const firstUsernameOrEmail = usernamesOrEmails[0]
            const lowercase = firstUsernameOrEmail?.toLowerCase() ?? null
            if (
                usernamesOrEmails.length === 1 &&
                lowercase != null &&
                !users.some(
                    user =>
                        user.name.toLowerCase() === lowercase ||
                        user.email.toLowerCase() === lowercase
                )
            ) {
                return users.filter(
                    newUser =>
                        newUser.name.toLowerCase().includes(lowercase) ||
                        newUser.email.toLowerCase().includes(lowercase)
                )
            } else {
                return users
            }
        }, [usernamesOrEmails, users])
        const willInviteNewUser = React.useMemo(() => {
            const firstUsernameOrEmail = usernamesOrEmails[0]
            if (firstUsernameOrEmail == null || firstUsernameOrEmail === '') {
                return true
            } else {
                const lowercase = firstUsernameOrEmail.toLowerCase()
                return (
                    lowercase !== '' &&
                    !usernamesOfUsersWithPermission.has(lowercase) &&
                    !emailsOfUsersWithPermission.has(lowercase) &&
                    !users.some(
                        innerUser =>
                            innerUser.name.toLowerCase() === lowercase ||
                            innerUser.email.toLowerCase() === lowercase
                    )
                )
            }
        }, [usernamesOrEmails, emailsOfUsersWithPermission, usernamesOfUsersWithPermission, users])

        const doSubmit = async () => {
            if (willInviteNewUser) {
                try {
                    const firstUsernameOrEmail = usernamesOrEmails[0]
                    setUsernamesOrEmails([])
                    if (firstUsernameOrEmail != null) {
                        await backend.inviteUser({
                            organizationId: organization.id,
                            userEmail: backendModule.EmailAddress(firstUsernameOrEmail),
                        })
                        toast.toast.success(`You've invited ${firstUsernameOrEmail} to join Enso!`)
                    }
                } catch (error) {
                    toastAndLog('Could not invite user', error)
                }
            } else {
                setUsernamesOrEmails([])
                const usersMap = Object.fromEntries(
                    users.flatMap(theUser => [
                        [theUser.name.toLowerCase(), theUser],
                        [theUser.email.toLowerCase(), theUser],
                    ])
                )
                const addedUsersPermissions = usernamesOrEmails
                    .flatMap(usernameOrEmail => {
                        const newUser = usersMap[usernameOrEmail]
                        return newUser != null ? [newUser] : []
                    })
                    .map<backendModule.UserPermission>(newUser => ({
                        user: {
                            // The names come from a third-party API and cannot be
                            // changed.
                            /* eslint-disable @typescript-eslint/naming-convention */
                            organization_id: organization.id,
                            pk: newUser.id,
                            user_email: newUser.email,
                            user_name: newUser.name,
                            /* eslint-enable @typescript-eslint/naming-convention */
                        },
                        permission: action,
                    }))
                const addedUsersPks = new Set(addedUsersPermissions.map(newUser => newUser.user.pk))
                const oldUsersPermissions = permissions
                try {
                    const newUsersPermissions = [
                        ...permissions.filter(
                            oldUserPermissions => !addedUsersPks.has(oldUserPermissions.user.pk)
                        ),
                        ...addedUsersPermissions,
                    ].sort(backendModule.compareUserPermissions)
                    setPermissions(newUsersPermissions)
                    await backend.createPermission({
                        userSubjects: addedUsersPermissions.map(
                            userPermissions => userPermissions.user.pk
                        ),
                        resourceId: item.id,
                        action: action,
                    })
                } catch (error) {
                    setPermissions(oldUsersPermissions)
                    const usernames = addedUsersPermissions.map(
                        userPermissions => userPermissions.user.user_name
                    )
                    toastAndLog(`Unable to set permissions for ${usernames.join(', ')}`, error)
                }
            }
        }

        const doDelete = async (userToDelete: backendModule.User) => {
            const oldUsersPermissions = permissions
            try {
                const newUsersPermissions = permissions.filter(
                    oldUserPermissions => oldUserPermissions.user.pk !== userToDelete.pk
                )
                setPermissions(newUsersPermissions)
                await backend.createPermission({
                    userSubjects: [userToDelete.pk],
                    resourceId: item.id,
                    action: null,
                })
            } catch (error) {
                setPermissions(oldUsersPermissions)
                toastAndLog(`Unable to set permissions of '${userToDelete.user_email}'`, error)
            }
        }

        const onlyUsernameOrEmail =
            usernamesOrEmails.length === 1 ? usernamesOrEmails[0] ?? null : null
        return (
            <Modal className="absolute overflow-hidden bg-dim w-full h-full top-0 left-0 z-10">
                <div
                    style={{
                        left: position.left + window.scrollX,
                        top: position.top + window.scrollY,
                    }}
                    className="sticky w-115.25"
                    onClick={mouseEvent => {
                        mouseEvent.stopPropagation()
                    }}
                    onContextMenu={mouseEvent => {
                        mouseEvent.stopPropagation()
                        mouseEvent.preventDefault()
                    }}
                >
                    <div className="absolute bg-frame-selected backdrop-blur-3xl rounded-2xl h-full w-full -z-10" />
                    <div className="flex flex-col rounded-2xl gap-2 p-2">
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
                            <div className="flex items-center grow rounded-full border border-black-a10 gap-2 px-1">
                                <PermissionSelector
                                    disabled={willInviteNewUser}
                                    action={backendModule.PermissionAction.view}
                                    assetType={item.type}
                                    onChange={setAction}
                                />
                                <input
                                    readOnly
                                    hidden
                                    ref={emailValidityRef}
                                    type="email"
                                    className="hidden"
                                    value={usernamesOrEmails[0] ?? ''}
                                />
                                <Autocomplete
                                    multiple
                                    maxItemsToShow={MAX_AUTOCOMPLETE_ITEMS_TO_SHOW}
                                    autoFocus
                                    placeholder="Type usernames or emails to search or invite"
                                    type="text"
                                    itemNamePlural="users"
                                    values={usernamesOrEmails}
                                    setValues={newUsernamesOrEmails => {
                                        setUsernamesOrEmails(
                                            newUsernamesOrEmails.map(
                                                usernameOrEmail =>
                                                    emailToUsername[usernameOrEmail] ??
                                                    usernameOrEmail
                                            )
                                        )
                                    }}
                                    items={matchingUsers.map(matchingUser =>
                                        onlyUsernameOrEmail != null &&
                                        !matchingUser.name.includes(onlyUsernameOrEmail)
                                            ? matchingUser.email
                                            : matchingUser.name
                                    )}
                                    className="grow"
                                    inputClassName="bg-transparent leading-170 h-6 py-px"
                                />
                            </div>
                            <button
                                type="submit"
                                disabled={
                                    usernamesOrEmails.length === 0 ||
                                    (onlyUsernameOrEmail != null &&
                                        usernamesOfUsersWithPermission.has(onlyUsernameOrEmail)) ||
                                    (onlyUsernameOrEmail != null &&
                                        emailsOfUsersWithPermission.has(onlyUsernameOrEmail)) ||
                                    (willInviteNewUser &&
                                        emailValidityRef.current?.validity.valid !== true)
                                }
                                className="text-tag-text bg-invite rounded-full px-2 py-1 disabled:opacity-30"
                            >
                                <div className="h-6 py-0.5">
                                    {willInviteNewUser ? 'Invite' : 'Share'}
                                </div>
                            </button>
                        </form>
                        <div className="overflow-auto pl-1 pr-12 max-h-80">
                            {permissions.map(userPermissions => (
                                <div
                                    key={userPermissions.user.pk}
                                    className="flex items-center h-8"
                                >
                                    <UserPermissions
                                        asset={item}
                                        userPermission={userPermissions}
                                        setUserPermission={newUserPermission => {
                                            setPermissions(
                                                permissions
                                                    .map(oldUserPermission =>
                                                        oldUserPermission.user.pk ===
                                                        newUserPermission.user.pk
                                                            ? newUserPermission
                                                            : oldUserPermission
                                                    )
                                                    .sort(backendModule.compareUserPermissions)
                                            )
                                        }}
                                        doDelete={doDelete}
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
