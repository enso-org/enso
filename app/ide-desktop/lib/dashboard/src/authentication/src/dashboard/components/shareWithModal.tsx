/** @file A modal with inputs for user email and permission level. */
import * as react from 'react'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as newtype from '../../newtype'
import * as svg from '../../components/svg'

import PermissionDisplay, * as permissionDisplay from './permissionDisplay'
import Autocomplete from './autocomplete'
import Modal from './modal'

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
    eventTarget: HTMLElement
}

/** A modal with inputs for user email and permission level. */
export function ShareWithModal(props: ShareWithModalProps) {
    const {
        asset: { type: assetType, id: assetId },
        eventTarget,
    } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [users, setUsers] = react.useState<backendModule.SimpleUser[]>([])
    const [matchingUsers, setMatchingUsers] = react.useState(users)
    const [canSubmit, setCanSubmit] = react.useState(true)
    const [email, setEmail] = react.useState('')
    const [permission, setPermission] = react.useState<backendModule.PermissionAction | null>(null)
    const [userEmailClassName, setUserEmailClassName] = react.useState<string | null>(null)
    const [permissionClassName, setPermissionClassName] = react.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        // This should never happen.
        return <></>
    } else {
        const position = eventTarget.getBoundingClientRect()

        react.useEffect(() => {
            void (async () => {
                const newUsers = await backend.listUsers()
                setUsers(newUsers)
                const lowercaseEmail = email.toLowerCase()
                setMatchingUsers(
                    newUsers.filter(newUser => newUser.email.toLowerCase().includes(lowercaseEmail))
                )
            })()
        }, [])

        const onSubmit = react.useCallback(async (formEvent: React.FormEvent) => {
            formEvent.preventDefault()
            const isEmailInvalid = !users.some(user => email === user.email)
            const isPermissionInvalid = permission == null
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
            } else {
                unsetModal()
                await backend.createPermission({
                    userSubject: newtype.asNewtype(email),
                    resourceId: assetId,
                    action: permission,
                })
            }
        }, [])

        // FIXME:
        const assetTypeName = assetType === backendModule.AssetType.directory ? 'folder' : assetType

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
                        {svg.CLOSE_ICON}
                    </button>
                    <h2 className="inline-block font-semibold m-2">Share {assetTypeName}</h2>
                    <div className="mx-2">
                        <label htmlFor="share_with_user_email">Email</label>
                        <Autocomplete
                            autoFocus
                            initialValue={email}
                            items={matchingUsers.map(user => user.email)}
                            className={userEmailClassName ?? ''}
                            onInput={newEmail => {
                                const lowercaseEmail = newEmail.toLowerCase()
                                setMatchingUsers(
                                    users.filter(user => user.email.includes(lowercaseEmail))
                                )
                            }}
                            onChange={newEmail => {
                                setEmail(newEmail)
                            }}
                        />
                    </div>
                    <div className="mx-2">Permission</div>
                    <div className="m-1">
                        <input
                            type="radio"
                            id="share_with_permission_own"
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onClick={() => {
                                setPermission(backendModule.PermissionAction.own)
                            }}
                        />
                        <label htmlFor="share_with_permission_own">
                            <PermissionDisplay
                                permissions={
                                    permissionDisplay.PERMISSION[backendModule.PermissionAction.own]
                                }
                                className={`cursor-pointer ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">Own</div>
                            </PermissionDisplay>
                        </label>
                        <input
                            type="radio"
                            id="share_with_permission_read"
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onClick={() => {
                                setPermission(backendModule.PermissionAction.read)
                            }}
                        />
                        <label htmlFor="share_with_permission_read">
                            <PermissionDisplay
                                permissions={
                                    permissionDisplay.PERMISSION[
                                        backendModule.PermissionAction.read
                                    ]
                                }
                                className={`cursor-pointer ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">Read</div>
                            </PermissionDisplay>
                        </label>
                        <input
                            type="radio"
                            id="share_with_permission_edit"
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onClick={() => {
                                setPermission(backendModule.PermissionAction.edit)
                            }}
                        />
                        <label htmlFor="share_with_permission_edit">
                            <PermissionDisplay
                                permissions={
                                    permissionDisplay.PERMISSION[
                                        backendModule.PermissionAction.edit
                                    ]
                                }
                                className={`cursor-pointer ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">Edit</div>
                            </PermissionDisplay>
                        </label>
                        <input
                            type="radio"
                            id="share_with_permission_execute"
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onClick={() => {
                                setPermission(backendModule.PermissionAction.execute)
                            }}
                        />
                        <label htmlFor="share_with_permission_execute">
                            <PermissionDisplay
                                permissions={
                                    permissionDisplay.PERMISSION[
                                        backendModule.PermissionAction.execute
                                    ]
                                }
                                className={`cursor-pointer ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">Execute</div>
                            </PermissionDisplay>
                        </label>
                    </div>
                    <input
                        type="submit"
                        disabled={!canSubmit}
                        className={`inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2 ${
                            canSubmit ? 'hover:cursor-pointer' : 'opacity-50'
                        }`}
                        value="Share"
                    />
                </form>
            </Modal>
        )
    }
}

export default ShareWithModal
