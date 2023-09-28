/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as app from '../../components/app'
import * as auth from '../../authentication/providers/auth'
import * as hooks from '../../hooks'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

import ChangePasswordModal from './changePasswordModal'
import MenuEntry from './menuEntry'
import Modal from './modal'

// ================
// === UserMenu ===
// ================

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
    onSignOut: () => void
}

/** Handling the UserMenuItem click event logic and displaying its content. */
export default function UserMenu(props: UserMenuProps) {
    const { onSignOut } = props
    const navigate = hooks.useNavigate()
    const { signOut } = auth.useAuth()
    const { accessToken, organization } = auth.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()

    // The shape of the JWT payload is statically known.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const username: string | null =
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
        accessToken != null ? JSON.parse(atob(accessToken.split('.')[1]!)).username : null
    const canChangePassword = username != null ? !/^Github_|^Google_/.test(username) : false

    return (
        <Modal className="absolute overflow-hidden bg-dim w-full h-full">
            <div
                className="absolute flex flex-col bg-frame-selected backdrop-blur-3xl rounded-2xl gap-3 right-2.25 top-2.25 w-51.5 px-2 py-2.25"
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                {organization != null ? (
                    <>
                        <div className="flex items-center gap-3 px-1">
                            <img src={DefaultUserIcon} height={28} width={28} />
                            <span className="leading-170 h-6 py-px">{organization.name}</span>
                        </div>
                        <div className="flex flex-col">
                            {canChangePassword && (
                                <MenuEntry
                                    action={shortcuts.KeyboardAction.changeYourPassword}
                                    paddingClassName="p-1"
                                    doAction={() => {
                                        setModal(<ChangePasswordModal />)
                                    }}
                                />
                            )}
                            <MenuEntry
                                action={shortcuts.KeyboardAction.signOut}
                                paddingClassName="p-1"
                                doAction={() => {
                                    onSignOut()
                                    // Wait until React has switched back to drive view, before signing out.
                                    window.setTimeout(() => {
                                        void signOut()
                                    }, 0)
                                }}
                            />
                        </div>
                    </>
                ) : (
                    <>
                        <div className="flex items-center h-7">
                            <span className="leading-170 h-6 py-px">You are not signed in.</span>
                        </div>
                        <div className="flex flex-col">
                            <MenuEntry
                                action={shortcuts.KeyboardAction.signIn}
                                paddingClassName="p-1"
                                doAction={() => {
                                    navigate(app.LOGIN_PATH)
                                }}
                            />
                        </div>
                    </>
                )}
            </div>
        </Modal>
    )
}
