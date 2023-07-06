/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'
import toast from 'react-hot-toast'

import CloseIcon from 'enso-assets/close.svg'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as modalProvider from '../../providers/modal'
import * as newtype from '../../newtype'

import Autocomplete from './autocomplete'
import Modal from './modal'
import PermissionSelector from './permissionSelector'

// =================
// === Constants ===
// =================

const BLINK_BACKGROUND_CLASS_NAME = 'blink-background'
/** The duration of the blink animation. This MUST be synced with the corresponding value in
 * `tailwind.css`. */
const BLINK_ANIMATION_LENGTH_MS = 500

// ======================
// === ShareWithModal ===
// ======================

/** Possible actions that a {@link ManagePermissionsModal} can perform. */
enum ManagePermissionsAction {
    /** The default action. Add permissions for a new user. */
    share = 'share',
    /** Update permissions. */
    update = 'update',
    /** Remove access for a user from this asset. */
    remove = 'remove',
    /** Invite a user not yet in the organization. */
    inviteToOrganization = 'invite-to-organization',
}

/** The text on the submit button, for each action. */
const SUBMIT_BUTTON_TEXT: Record<ManagePermissionsAction, string> = {
    [ManagePermissionsAction.share]: 'Share',
    [ManagePermissionsAction.update]: 'Update',
    [ManagePermissionsAction.remove]: 'Remove',
    [ManagePermissionsAction.inviteToOrganization]: 'Invite to organization',
} as const

/** The classes specific to each action, for the submit button. */
const ACTION_CSS_CLASS: Record<ManagePermissionsAction, string> = {
    [ManagePermissionsAction.share]: 'bg-blue-600',
    [ManagePermissionsAction.update]: 'bg-blue-600',
    [ManagePermissionsAction.remove]: 'bg-red-700',
    [ManagePermissionsAction.inviteToOrganization]: 'bg-blue-600',
} as const

/** Props for a {@link ManagePermissionsModal}. */
export interface ManagePermissionsModalProps {
    asset: backendModule.Asset
    /* If present, the user cannot be changed. */
    user?: backendModule.User
    onSuccess: () => void
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export function ManagePermissionsModal(props: ManagePermissionsModalProps) {
    const { asset, user: rawUser, onSuccess, eventTarget } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [willInviteNewUser, setWillInviteNewUser] = React.useState(false)
    const [users, setUsers] = React.useState<backendModule.SimpleUser[]>([])
    const [matchingUsers, setMatchingUsers] = React.useState(users)
    const [canSubmit, setCanSubmit] = React.useState(true)
    const [email, setEmail] = React.useState<string>(rawUser?.user_email ?? '')
    const [permissions, setPermissions] = React.useState(new Set<backendModule.PermissionAction>())
    const [userEmailClassName, setUserEmailClassName] = React.useState<string | null>(null)
    const [permissionClassName, setPermissionClassName] = React.useState<string | null>(null)

    const user = React.useMemo(
        () =>
            rawUser ??
            (asset.permissions ?? []).find(permission => permission.user.user_email === email)
                ?.user,
        [rawUser, email, /* should never change */ asset.permissions]
    )

    const action =
        user != null
            ? permissions.size !== 0
                ? ManagePermissionsAction.update
                : ManagePermissionsAction.remove
            : willInviteNewUser
            ? ManagePermissionsAction.inviteToOrganization
            : ManagePermissionsAction.share

    /** Overridden by the user's permissions only if it is not empty. */
    const initialPermissions = React.useMemo(
        () =>
            permissions.size !== 0
                ? null
                : user != null
                ? new Set(
                      (asset.permissions ?? [])
                          .filter(
                              assetPermission => assetPermission.user.user_email === user.user_email
                          )
                          .map(userPermission => userPermission.permission)
                  )
                : null,
        // `permissions` is NOT a dependency; this is an expensive computation so it is only used
        // to determine whether the computation should be avoided completely.
        // eslint-disable-next-line react-hooks/exhaustive-deps
        [user, /* should never change */ asset.permissions]
    )

    const emailsOfUsersWithPermission = React.useMemo(
        () =>
            new Set(asset.permissions?.map(userPermission => userPermission.user.user_email) ?? []),
        [/* should never change */ asset.permissions]
    )

    // This is INCORRECT, but SAFE to use in hooks as its value will be set by the time any hook
    // runs.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const userEmailRef = React.useRef<HTMLInputElement>(null!)

    if (backend.type === backendModule.BackendType.local || organization == null) {
        // This should never happen - the local backend does not have the "shared with" column,
        // and `organization` is absent only when offline - in which case the user should only
        // be able to access the local backend.
        // This MUST be an error, otherwise the hooks below are considered as conditionally called.
        throw new Error('Unable to share projects on the local backend.')
    } else {
        const position = eventTarget.getBoundingClientRect()

        React.useEffect(() => {
            void (async () => {
                const listedUsers = await backend.listUsers()
                const newUsers = listedUsers.filter(
                    listedUser => !emailsOfUsersWithPermission.has(listedUser.email)
                )
                setUsers(newUsers)
                const lowercaseEmail = email.toLowerCase()
                setMatchingUsers(
                    newUsers.filter(newUser => newUser.email.toLowerCase().includes(lowercaseEmail))
                )
            })()
            // `email` is NOT a dependency. `matchingUsers` is updated based on `email` elsewhere.
        }, [
            email,
            /* should never change */ backend,
            /* should never change */ emailsOfUsersWithPermission,
        ])

        const onEmailChange = React.useCallback(
            (newEmail: string) => {
                setEmail(newEmail)
                const lowercaseEmail = newEmail.toLowerCase()
                if (userEmailRef.current.validity.valid) {
                    // A new user will be invited to the organization.
                    setWillInviteNewUser(
                        lowercaseEmail !== '' &&
                            !emailsOfUsersWithPermission.has(
                                newtype.asNewtype<backendModule.EmailAddress>(lowercaseEmail)
                            ) &&
                            !users.some(
                                innerUser => innerUser.email.toLowerCase() === lowercaseEmail
                            )
                    )
                }
            },
            [users, /* should never change */ emailsOfUsersWithPermission]
        )

        const onSubmit = React.useCallback(
            async (formEvent: React.FormEvent) => {
                formEvent.preventDefault()
                const isEmailInvalid = !userEmailRef.current.validity.valid
                const isPermissionsInvalid =
                    !willInviteNewUser && user == null && permissions.size === 0
                const lowercaseEmail = email.toLowerCase()
                const finalUser: backendModule.SimpleUser | null =
                    user != null
                        ? { id: user.pk, email: user.user_email, name: user.user_name }
                        : willInviteNewUser
                        ? null
                        : users.find(theUser => theUser.email.toLowerCase() === lowercaseEmail) ??
                          null
                if (isEmailInvalid || isPermissionsInvalid) {
                    if (isEmailInvalid) {
                        setUserEmailClassName(BLINK_BACKGROUND_CLASS_NAME)
                        window.setTimeout(() => {
                            setUserEmailClassName('')
                        }, BLINK_ANIMATION_LENGTH_MS)
                    }
                    if (isPermissionsInvalid) {
                        setPermissionClassName(BLINK_BACKGROUND_CLASS_NAME)
                        window.setTimeout(() => {
                            setPermissionClassName('')
                        }, BLINK_ANIMATION_LENGTH_MS)
                    }
                    setCanSubmit(false)
                    window.setTimeout(() => {
                        setCanSubmit(true)
                    }, BLINK_ANIMATION_LENGTH_MS)
                } else if (willInviteNewUser) {
                    unsetModal()
                    try {
                        await backend.inviteUser({
                            organizationId: organization.id,
                            userEmail: newtype.asNewtype<backendModule.EmailAddress>(email),
                        })
                    } catch (error) {
                        toast.error(errorModule.tryGetMessage(error) ?? 'Unknown error.')
                    }
                } else if (finalUser != null) {
                    unsetModal()
                    try {
                        await backend.createPermission({
                            userSubjects: [finalUser.id],
                            resourceId: asset.id,
                            actions: [...permissions],
                        })
                        onSuccess()
                    } catch (error) {
                        toast.error(`Unable to set permissions of '${finalUser.email}'.`)
                    }
                }
            },
            [
                email,
                permissions,
                willInviteNewUser,
                asset.id,
                organization.id,
                users,
                onSuccess,
                /* should never change */ unsetModal,
                /* should never change */ backend,
                /* should never change */ user,
            ]
        )

        return (
            <Modal className="absolute overflow-hidden bg-opacity-25 w-full h-full top-0 left-0 z-10">
                <form
                    style={{
                        left: position.left + window.scrollX,
                        top: position.top + window.scrollY,
                    }}
                    className="sticky bg-white shadow-soft rounded-lg w-64"
                    onSubmit={onSubmit}
                    onClick={mouseEvent => {
                        mouseEvent.stopPropagation()
                    }}
                >
                    <button type="button" className="absolute right-0 m-2" onClick={unsetModal}>
                        <img src={CloseIcon} />
                    </button>
                    <h2 className="inline-block font-semibold m-2">
                        {user == null ? 'Share' : 'Update permissions'}
                    </h2>
                    <div className="mx-2">
                        <label htmlFor="share_with_user_email">Email</label>
                        <Autocomplete
                            autoFocus
                            disabled={rawUser != null}
                            inputRef={userEmailRef}
                            type="email"
                            initialValue={email}
                            items={matchingUsers.map(matchingUser => matchingUser.email)}
                            className={userEmailClassName ?? ''}
                            onInput={newEmail => {
                                const lowercaseEmail = newEmail.toLowerCase()
                                setMatchingUsers(
                                    users.filter(innerUser =>
                                        innerUser.email.includes(lowercaseEmail)
                                    )
                                )
                            }}
                            onChange={onEmailChange}
                        />
                    </div>
                    {!willInviteNewUser && (
                        <>
                            <div className="mx-2">Permission</div>
                            <PermissionSelector
                                className="m-1"
                                initialPermissions={initialPermissions}
                                permissionClassName={permissionClassName ?? ''}
                                onChange={setPermissions}
                            />
                        </>
                    )}
                    <input
                        type="submit"
                        disabled={!canSubmit}
                        className={`inline-block text-white rounded-full px-4 py-1 m-2 ${
                            ACTION_CSS_CLASS[action]
                        } ${canSubmit ? 'hover:cursor-pointer' : 'opacity-50'}`}
                        value={SUBMIT_BUTTON_TEXT[action]}
                    />
                </form>
            </Modal>
        )
    }
}

export default ManagePermissionsModal
