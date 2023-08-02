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
    const [usernameOrEmail, setUsernameOrEmail] = React.useState(rawUser?.user_name ?? '')
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
            const lowercase = usernameOrEmail.toLowerCase()
            return users.filter(
                newUser =>
                    newUser.name.toLowerCase().includes(lowercase) ||
                    newUser.email.toLowerCase().includes(lowercase)
            )
        }, [usernameOrEmail, users])
        const willInviteNewUser = React.useMemo(() => {
            if (usernameOrEmail === '') {
                return true
            } else {
                const lowercase = usernameOrEmail.toLowerCase()
                return (
                    emailValidityRef.current?.checkValidity() === true &&
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
        }, [usernameOrEmail, emailsOfUsersWithPermission, usernamesOfUsersWithPermission, users])

        const doSubmit = async () => {
            if (willInviteNewUser) {
                try {
                    setUsernameOrEmail('')
                    await backend.inviteUser({
                        organizationId: organization.id,
                        userEmail: backendModule.EmailAddress(usernameOrEmail),
                    })
                    toast.toast.success(`You've invited ${usernameOrEmail} to join Enso!`)
                } catch (error) {
                    toastAndLog('Could not invite user', error)
                }
            } else {
                setUsernameOrEmail('')
                const newUser = users.find(
                    currentUser =>
                        currentUser.name === usernameOrEmail ||
                        currentUser.email === usernameOrEmail
                )
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
                            ...usersPermissions.map(oldUserPermissions => {
                                if (oldUserPermissions.user.pk === userPermissions.user.pk) {
                                    found = true
                                    return userPermissions
                                } else {
                                    return oldUserPermissions
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
                                    value={usernameOrEmail}
                                />
                                <Autocomplete
                                    autoFocus
                                    placeholder="Type usernames or emails to search or invite"
                                    disabled={rawUser != null}
                                    type="text"
                                    initialValue={usernameOrEmail}
                                    items={matchingUsers.map(matchingUser =>
                                        matchingUser.name.includes(usernameOrEmail)
                                            ? matchingUser.name
                                            : matchingUser.email
                                    )}
                                    className="grow"
                                    inputClassName="bg-transparent h-6 py-px"
                                    onChange={setUsernameOrEmail}
                                />
                            </div>
                            <button
                                disabled={
                                    usernameOrEmail === '' ||
                                    emailValidityRef.current?.checkValidity() !== true
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
