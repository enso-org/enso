/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as permissionsModule from '../permissions'

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
    asset: backendModule.Asset
    initialPermissions: permissionsModule.Permissions
    usersPermissions: backendModule.UserPermissions[]
    setUsersPermissions: (usersPermissions: backendModule.UserPermissions[]) => void
    /* If present, the user cannot be changed. */
    user?: backendModule.User
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManagePermissionsModal(props: ManagePermissionsModalProps) {
    const {
        asset,
        initialPermissions,
        usersPermissions: initialUsersPermissions,
        setUsersPermissions: outerSetUsersPermissions,
        user: rawUser,
        eventTarget,
    } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [usernamesOrEmails, setUsernamesOrEmails] = React.useState(
        rawUser != null ? [rawUser.user_name] : []
    )
    const [permissions, setPermissions] = React.useState(initialPermissions)
    const emailValidityRef = React.useRef<HTMLInputElement>(null)
    const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
    const [usersPermissions, setUsersPermissions] = React.useState(initialUsersPermissions)
    const usernamesOfUsersWithPermission = React.useMemo(
        () => new Set(usersPermissions.map(userPermission => userPermission.user.user_name)),
        [usersPermissions]
    )
    const emailsOfUsersWithPermission = React.useMemo(
        () =>
            new Set<string>(usersPermissions.map(userPermission => userPermission.user.user_email)),
        [usersPermissions]
    )

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
                    .map<backendModule.UserPermissions>(newUser => ({
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
                        permissions,
                    }))
                const addedUsersPermissionsMap = new Map(
                    addedUsersPermissions.map(newUser => [newUser.user.pk, newUser])
                )
                const oldUsersPermissions = usersPermissions
                try {
                    const newUsersPermissions = [
                        ...usersPermissions.map(oldUserPermissions => {
                            const newUserPermissions = addedUsersPermissionsMap.get(
                                oldUserPermissions.user.pk
                            )
                            if (newUserPermissions != null) {
                                addedUsersPermissionsMap.delete(oldUserPermissions.user.pk)
                                return newUserPermissions
                            } else {
                                return oldUserPermissions
                            }
                        }),
                        ...addedUsersPermissionsMap.values(),
                    ]
                    setUsersPermissions(newUsersPermissions)
                    outerSetUsersPermissions(newUsersPermissions)
                    await backend.createPermission({
                        userSubjects: Object.values(usersMap).map(user => user.id),
                        resourceId: asset.id,
                        actions: backendModule.permissionsToPermissionActions(permissions),
                    })
                } catch (error) {
                    setUsersPermissions(oldUsersPermissions)
                    outerSetUsersPermissions(oldUsersPermissions)
                    const userEmails = Object.values(usersMap).map(user => `'${user.email}'`)
                    toastAndLog(`Unable to set permissions of ${userEmails.join(', ')}`, error)
                }
            }
        }

        const doDelete = async (userToDelete: backendModule.User) => {
            const oldUsersPermissions = usersPermissions
            try {
                const newUsersPermissions = usersPermissions.filter(
                    oldUserPermissions => oldUserPermissions.user.pk !== userToDelete.pk
                )
                setUsersPermissions(newUsersPermissions)
                outerSetUsersPermissions(newUsersPermissions)
                await backend.createPermission({
                    userSubjects: [userToDelete.pk],
                    resourceId: asset.id,
                    actions: [],
                })
            } catch (error) {
                setUsersPermissions(oldUsersPermissions)
                outerSetUsersPermissions(oldUsersPermissions)
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
                        <div className="flex gap-1">
                            <div className="flex items-center grow rounded-full border border-black-a10 gap-2 px-1">
                                <PermissionSelector
                                    disabled={willInviteNewUser}
                                    initialPermissions={initialPermissions}
                                    assetType={asset.type}
                                    onChange={setPermissions}
                                />
                                <input
                                    readOnly
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
                                    disabled={rawUser != null}
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
                                        matchingUser.email.includes(onlyUsernameOrEmail)
                                            ? matchingUser.email
                                            : matchingUser.name
                                    )}
                                    className="grow"
                                    inputClassName="bg-transparent leading-170 h-6 py-px"
                                />
                            </div>
                            <button
                                disabled={
                                    usernamesOrEmails.length === 0 ||
                                    (willInviteNewUser &&
                                        emailValidityRef.current?.validity.valid !== true)
                                }
                                className="text-tag-text bg-invite rounded-full px-2 py-1 disabled:opacity-30"
                                onClick={doSubmit}
                            >
                                <div className="h-6 py-0.5">
                                    {willInviteNewUser ? 'Invite' : 'Share'}
                                </div>
                            </button>
                        </div>
                        <div className="pl-1 pr-12">
                            {usersPermissions.map(userPermissions => (
                                <div
                                    key={userPermissions.user.pk}
                                    className="flex items-center h-8"
                                >
                                    <UserPermissions
                                        asset={asset}
                                        userPermissions={userPermissions}
                                        setUserPermissions={newUserPermissions => {
                                            setUsersPermissions(oldUsersPermissions => {
                                                const newUsersPermissions = oldUsersPermissions.map(
                                                    oldUserPermissions =>
                                                        oldUserPermissions.user.pk ===
                                                        newUserPermissions.user.pk
                                                            ? newUserPermissions
                                                            : oldUserPermissions
                                                )
                                                setTimeout(() => {
                                                    outerSetUsersPermissions(newUsersPermissions)
                                                }, 0)
                                                return newUsersPermissions
                                            })
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
