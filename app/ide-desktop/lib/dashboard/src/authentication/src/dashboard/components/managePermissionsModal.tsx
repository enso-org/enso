/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as permissionsModule from '../permissions'

import Autocomplete from './autocomplete'
import Modal from './modal'
import PermissionSelector from './permissionSelector'
import UserPermissions from './userPermissions'

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
    const [users, setUsers] = React.useState<backendModule.SimpleUser[]>([])
    const [matchingUsers, setMatchingUsers] = React.useState(users)
    const [email, setEmail] = React.useState<string | null>(rawUser?.user_email ?? null)
    const [permissions, setPermissions] = React.useState(initialPermissions)
    const userEmailRef = React.useRef<HTMLInputElement>(null)
    const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
    const [usersPermissions, setUsersPermissions] = React.useState(initialUsersPermissions)
    const emailsOfUsersWithPermission = React.useMemo(
        () =>
            new Set(usersPermissions.map<string>(userPermission => userPermission.user.user_email)),
        [usersPermissions]
    )
    const usernamesOfUsersWithPermission = React.useMemo(
        () =>
            new Set(usersPermissions.map<string>(userPermission => userPermission.user.user_name)),
        [usersPermissions]
    )

    const willInviteNewUser = React.useMemo(() => {
        if (email == null || email === '') {
            return true
        } else {
            const lowercaseUser = email.toLowerCase()
            return (
                userEmailRef.current?.validity.valid === true &&
                lowercaseUser !== '' &&
                !emailsOfUsersWithPermission.has(lowercaseUser) &&
                !usernamesOfUsersWithPermission.has(lowercaseUser) &&
                !users.some(innerUser => innerUser.email.toLowerCase() === lowercaseUser)
            )
        }
    }, [email, emailsOfUsersWithPermission, usernamesOfUsersWithPermission, users])

    const user = React.useMemo(() => {
        if (rawUser != null) {
            return rawUser
        } else if (email != null) {
            return asset.permissions?.find(permission => permission.user.user_email === email)?.user
        } else {
            return null
        }
    }, [rawUser, email, /* should never change */ asset.permissions])

    if (backend.type === backendModule.BackendType.local || organization == null) {
        // This should never happen - the local backend does not have the "shared with" column,
        // and `organization` is absent only when offline - in which case the user should only
        // be able to access the local backend.
        // This MUST be an error, otherwise the hooks below are considered as conditionally called.
        throw new Error('Unable to share projects on the local backend.')
    } else {
        React.useEffect(() => {
            if (user == null) {
                void (async () => {
                    const listedUsers = await backend.listUsers()
                    const newUsers = listedUsers.filter(
                        listedUser => !emailsOfUsersWithPermission.has(listedUser.email)
                    )
                    setUsers(newUsers)
                    const lowercaseEmail = email?.toLowerCase() ?? ''
                    setMatchingUsers(
                        newUsers.filter(newUser =>
                            newUser.email.toLowerCase().includes(lowercaseEmail)
                        )
                    )
                })()
            }
            // `emails` is NOT a dependency. `matchingUsers` is updated based on `email` elsewhere.
            // eslint-disable-next-line react-hooks/exhaustive-deps
        }, [
            rawUser,
            /* should never change */ backend,
            /* should never change */ emailsOfUsersWithPermission,
        ])

        const doSubmit = async () => {
            if (willInviteNewUser) {
                try {
                    // If the email address is `null`, then the "Invite" button should be disabled,
                    // meaning this callback should never run.
                    if (email != null) {
                        setEmail(null)
                        await backend.inviteUser({
                            organizationId: organization.id,
                            userEmail: backendModule.EmailAddress(email),
                        })
                    }
                } catch (error) {
                    toastAndLog('Could not invite user', error)
                }
            } else {
                setEmail(null)
                const newUser = users.find(currentUser => currentUser.email === email)
                if (newUser != null) {
                    const userPermissions: backendModule.UserPermissions = {
                        user: {
                            // The names come from a third-party API and cannot be changed.
                            /* eslint-disable @typescript-eslint/naming-convention */
                            organization_id: organization.id,
                            pk: newUser.id,
                            user_email: newUser.email,
                            user_name: newUser.name,
                            /* eslint-enable @typescript-eslint/naming-convention */
                        },
                        permissions,
                    }
                    const oldUsersPermissions = usersPermissions
                    try {
                        // This type assertion is SAFE. It is required to avoid TypeScript's overly
                        // eager narrowing.
                        // eslint-disable-next-line no-restricted-syntax
                        let found = false as boolean
                        const newUsersPermissions = [
                            ...usersPermissions.map(oldUserPermission => {
                                if (oldUserPermission.user.pk === userPermissions.user.pk) {
                                    found = true
                                    return userPermissions
                                } else {
                                    return oldUserPermission
                                }
                            }),
                            ...(found ? [] : [userPermissions]),
                        ]
                        setUsersPermissions(newUsersPermissions)
                        outerSetUsersPermissions(newUsersPermissions)
                        await backend.createPermission({
                            userSubjects: [userPermissions.user.pk],
                            resourceId: asset.id,
                            actions: backendModule.permissionsToPermissionActions(permissions),
                        })
                    } catch (error) {
                        setUsersPermissions(oldUsersPermissions)
                        outerSetUsersPermissions(oldUsersPermissions)
                        toastAndLog(`Unable to set permissions of '${newUser.email}'`, error)
                    }
                }
            }
        }

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
                            {/* Space reserved for other tabs */}
                        </div>
                        <div className="flex gap-1">
                            <div className="flex items-center grow rounded-full border border-black-a10 gap-2 px-1">
                                <PermissionSelector
                                    disabled={willInviteNewUser}
                                    initialPermissions={initialPermissions}
                                    assetType={asset.type}
                                    onChange={setPermissions}
                                />
                                <Autocomplete
                                    autoFocus
                                    placeholder="Type usernames or emails to search or invite"
                                    disabled={rawUser != null}
                                    inputRef={userEmailRef}
                                    type="email"
                                    initialValue={email ?? null}
                                    items={matchingUsers.map(matchingUser => matchingUser.email)}
                                    className="grow"
                                    inputClassName="bg-transparent h-6 py-px"
                                    onInput={newEmail => {
                                        const lowercaseEmail = newEmail.toLowerCase()
                                        setMatchingUsers(
                                            users.filter(innerUser =>
                                                innerUser.email.includes(lowercaseEmail)
                                            )
                                        )
                                    }}
                                    onChange={setEmail}
                                />
                            </div>
                            <button
                                disabled={
                                    email == null ||
                                    email === '' ||
                                    userEmailRef.current?.validity.valid !== true
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
