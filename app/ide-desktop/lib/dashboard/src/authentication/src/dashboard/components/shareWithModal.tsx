/** @file A modal with inputs for user email and permission level. */
import * as react from 'react'
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

/** Props for a {@link ShareWithModal}. */
export interface ShareWithModalProps {
    asset: backendModule.Asset
    /* If present, the user cannot be changed. */
    user?: backendModule.User
    onSuccess: () => void
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level. */
export function ShareWithModal(props: ShareWithModalProps) {
    const {
        asset: { type: assetType, id: assetId },
        user,
        onSuccess,
        eventTarget,
    } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [willInviteNewUser, setWillInviteNewUser] = react.useState(false)
    const [users, setUsers] = react.useState<backendModule.SimpleUser[]>([])
    const [matchingUsers, setMatchingUsers] = react.useState(users)
    const [canSubmit, setCanSubmit] = react.useState(true)
    const [email, setEmail] = react.useState<string>(user?.user_email ?? '')
    const [permission, setPermission] = react.useState<backendModule.PermissionAction | null>(null)
    const [userEmailClassName, setUserEmailClassName] = react.useState<string | null>(null)
    const [permissionClassName, setPermissionClassName] = react.useState<string | null>(null)

    // This is INCORRECT, but SAFE to use in hooks as its value will be set by the time any hook
    // runs.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const userEmailRef = react.useRef<HTMLInputElement>(null!)

    if (backend.type === backendModule.BackendType.local || organization == null) {
        // This should never happen - the local backend does not have the "shared with" column,
        // and `organization` is absent only when offline - in which case the user should only
        // be able to access the local backend.
        return <></>
    } else {
        const position = eventTarget.getBoundingClientRect()

        react.useEffect(() => {
            void (async () => {
                const listedUsers = await backend.listUsers()
                const newUsers = listedUsers.filter(
                    listedUser => listedUser.email !== organization.email
                )
                setUsers(newUsers)
                const lowercaseEmail = email.toLowerCase()
                setMatchingUsers(
                    newUsers.filter(newUser => newUser.email.toLowerCase().includes(lowercaseEmail))
                )
            })()
            // `email` is NOT a dependency. `matchingUsers` is updated based on `email` elsewhere.
        }, [])

        const onEmailChange = react.useCallback(
            (newEmail: string) => {
                setEmail(newEmail)
                const lowercaseEmail = newEmail.toLowerCase()
                if (userEmailRef.current.validity.valid) {
                    // A new user will be invited to the organization.
                    setWillInviteNewUser(
                        lowercaseEmail !== '' &&
                            !users.some(
                                innerUser => innerUser.email.toLowerCase() === lowercaseEmail
                            )
                    )
                }
            },
            [users]
        )

        const onSubmit = react.useCallback(
            async (formEvent: React.FormEvent) => {
                formEvent.preventDefault()
                const isEmailInvalid = !userEmailRef.current.validity.valid
                const isPermissionInvalid = !willInviteNewUser && permission == null
                const lowercaseEmail = email.toLowerCase()
                const finalUser: backendModule.SimpleUser | null =
                    user != null
                        ? { id: user.pk, email: user.user_email, name: user.user_name }
                        : willInviteNewUser
                        ? null
                        : users.find(theUser => theUser.email.toLowerCase() === lowercaseEmail) ??
                          null
                if (isEmailInvalid || isPermissionInvalid) {
                    if (isEmailInvalid) {
                        setUserEmailClassName(BLINK_BACKGROUND_CLASS_NAME)
                        window.setTimeout(() => {
                            setUserEmailClassName('')
                        }, BLINK_ANIMATION_LENGTH_MS)
                    }
                    if (isPermissionInvalid) {
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
                        toast.error(
                            `Unable to invite user '${email}': ${
                                errorModule.tryGetMessage(error) ?? 'unknown error.'
                            }`
                        )
                    }
                } else if (finalUser != null && permission != null) {
                    unsetModal()
                    try {
                        await backend.createPermission({
                            userSubject: finalUser.id,
                            resourceId: assetId,
                            action: permission,
                        })
                        onSuccess()
                    } catch (error) {
                        toast.error(
                            `Unable to give permission '${permission}' to '${finalUser.email}': ${
                                errorModule.tryGetMessage(error) ?? 'unknown error'
                            }.`
                        )
                    }
                }
            },
            [email, permission, willInviteNewUser, /* should never change */ user]
        )

        return (
            <Modal className="absolute overflow-hidden bg-opacity-25 w-full h-full top-0 left-0">
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
                        Share {backendModule.ASSET_TYPE_NAME[assetType]}
                    </h2>
                    <div className="mx-2">
                        <label htmlFor="share_with_user_email">Email</label>
                        <Autocomplete
                            autoFocus
                            disabled={user != null}
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
                    {willInviteNewUser ? (
                        <input
                            type="submit"
                            disabled={!canSubmit}
                            className={`inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2 ${
                                canSubmit ? 'hover:cursor-pointer' : 'opacity-50'
                            }`}
                            value="Invite user to organization"
                        />
                    ) : (
                        <>
                            <div className="mx-2">Permission</div>
                            <PermissionSelector
                                className="m-1"
                                permissionClassName={permissionClassName ?? ''}
                                onChange={setPermission}
                            />
                            <input
                                type="submit"
                                disabled={!canSubmit}
                                className={`inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2 ${
                                    canSubmit ? 'hover:cursor-pointer' : 'opacity-50'
                                }`}
                                value="Share"
                            />
                        </>
                    )}
                </form>
            </Modal>
        )
    }
}

export default ShareWithModal
