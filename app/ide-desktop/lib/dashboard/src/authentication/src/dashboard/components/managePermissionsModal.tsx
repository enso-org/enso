/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import CloseIcon from 'enso-assets/close.svg'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as modalProvider from '../../providers/modal'
import * as permissionsModule from '../permissions'

import Autocomplete from './autocomplete'
import Modal from './modal'
import PermissionSelector from './permissionSelector'

// ==============================
// === ManagePermissionsModal ===
// ==============================

/** Props for a {@link ManagePermissionsModal}. */
export interface ManagePermissionsModalProps {
    asset: backendModule.Asset
    initialPermissions: permissionsModule.Permissions
    emailsOfUsersWithPermission: Set<backendModule.EmailAddress>
    /* If present, the user cannot be changed. */
    user?: backendModule.User
    onSubmit: (users: backendModule.SimpleUser, permissions: permissionsModule.Permissions) => void
    onSuccess?: (
        users: backendModule.SimpleUser,
        permissions: permissionsModule.Permissions
    ) => void
    onFailure?: (
        users: backendModule.SimpleUser,
        permissions: permissionsModule.Permissions
    ) => void
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManagePermissionsModal(props: ManagePermissionsModalProps) {
    const {
        asset,
        initialPermissions,
        emailsOfUsersWithPermission,
        user: rawUser,
        onSubmit: rawOnSubmit,
        onSuccess,
        onFailure,
        eventTarget,
    } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
    const [users, setUsers] = React.useState<backendModule.SimpleUser[]>([])
    const [matchingUsers, setMatchingUsers] = React.useState(users)
    const [email, setEmail] = React.useState<string | null>(rawUser?.user_email ?? null)
    const [permissions, setPermissions] = React.useState(initialPermissions)

    // FIXME: if this is true, disable the permission selector
    const willInviteNewUser = React.useMemo(() => {
        if (email == null) {
            return false
        } else {
            const lowercaseEmail = backendModule.EmailAddress(email.toLowerCase())
            return (
                userEmailRef.current?.validity.valid === true &&
                lowercaseEmail !== '' &&
                !emailsOfUsersWithPermission.has(lowercaseEmail) &&
                !users.some(innerUser => innerUser.email.toLowerCase() === lowercaseEmail)
            )
        }
    }, [email, emailsOfUsersWithPermission, users])

    const user = React.useMemo(() => {
        if (rawUser != null) {
            return rawUser
        } else if (email != null) {
            return asset.permissions?.find(permission => permission.user.user_email === email)?.user
        } else {
            return null
        }
    }, [rawUser, email, /* should never change */ asset.permissions])

    const userEmailRef = React.useRef<HTMLInputElement>(null)

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

        const onSubmit = React.useCallback(
            async (formEvent: React.FormEvent) => {
                formEvent.preventDefault()
                const isEmailInvalid = userEmailRef.current?.validity.valid === false
                const usersMap = Object.fromEntries(
                    users.map(theUser => [theUser.email.toLowerCase(), theUser])
                )
                const finalUser: backendModule.SimpleUser | null =
                    rawUser != null
                        ? { id: rawUser.pk, email: rawUser.user_email, name: rawUser.user_name }
                        : email != null
                        ? usersMap[email.toLowerCase()] ?? null
                        : null
                if (isEmailInvalid) {
                    // This should never happen. Do nothing.
                } else if (finalUser != null) {
                    unsetModal()
                    try {
                        rawOnSubmit(finalUser, permissions)
                        await backend.createPermission({
                            userSubjects: [finalUser.id],
                            resourceId: asset.id,
                            actions: permissions,
                        })
                        onSuccess?.(finalUser, permissions)
                    } catch {
                        onFailure?.(finalUser, permissions)
                        toastify.toast.error(`Unable to set permissions of '${finalUser.email}'.`)
                    }
                }
            },
            [
                email,
                permissions,
                asset.id,
                users,
                rawOnSubmit,
                onSuccess,
                onFailure,
                /* should never change */ unsetModal,
                /* should never change */ backend,
                /* should never change */ rawUser,
            ]
        )

        const doInviteUser = React.useCallback(async () => {
            try {
                // If the email address is `null`, then the "Invite" button should be disabled,
                // meaning this callback should never run.
                if (email != null) {
                    await backend.inviteUser({
                        organizationId: organization.id,
                        userEmail: backendModule.EmailAddress(email),
                    })
                }
            } catch (error) {
                toastify.toast.error(errorModule.tryGetMessage(error) ?? 'Unknown error.')
            }
        }, [backend, email, organization.id])

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
                                    initialPermissions={initialPermissions}
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
                                disabled={!willInviteNewUser}
                                className="text-tag-text bg-invite rounded-full px-2 py-1 disabled:opacity-30"
                                onClick={doInviteUser}
                            >
                                <div className="h-6 py-0.5">Invite</div>
                            </button>
                        </div>
                        {Array.from(emailsOfUsersWithPermission, email => {})}
                    </div>
                </div>
            </Modal>
        )
    }
}
