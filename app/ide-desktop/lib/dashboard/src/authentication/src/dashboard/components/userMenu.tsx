/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as react from 'react'

import * as auth from '../../authentication/providers/auth'
import * as modalProvider from '../../providers/modal'

import ChangePasswordModal from './changePasswordModal'

// ================
// === UserMenu ===
// ================

/** This is the UI component for a `UserMenu` list item.
 * The main interaction logic is in the `onClick` injected by `UserMenu`. */
interface UserMenuItemProps {
    onClick?: React.MouseEventHandler<HTMLDivElement>
}

function UserMenuItem(props: react.PropsWithChildren<UserMenuItemProps>) {
    const { children, onClick } = props

    return (
        <div
            className={`whitespace-nowrap px-4 py-2 ${
                onClick ? 'hover:bg-blue-500 hover:text-white cursor-pointer' : ''
            }`}
            onClick={onClick}
        >
            {children}
        </div>
    )
}

/** Handling the UserMenuItem click event logic and displaying its content. */
function UserMenu() {
    const { signOut } = auth.useAuth()
    const { accessToken, organization } = auth.useFullUserSession()

    const { setModal } = modalProvider.useSetModal()

    const goToProfile = () => {
        // TODO: Implement this when the backend endpoints are implemented.
    }

    // We know the shape of the JWT payload.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
    const username: string = JSON.parse(atob(accessToken.split('.')[1]!)).username
    const canChangePassword = !/^Github_|^Google_/.test(username)

    return (
        <div
            className="absolute right-2 top-11 z-10 flex flex-col rounded-md bg-white py-1 border"
            onClick={event => {
                event.stopPropagation()
            }}
        >
            {/* FIXME[sb]: Figure out whether this conditional is *actually* needed. */}
            {/* eslint-disable-next-line @typescript-eslint/no-unnecessary-condition */}
            {organization ? (
                <>
                    <UserMenuItem>
                        Signed in as <span className="font-bold">{organization.name}</span>
                    </UserMenuItem>
                    <UserMenuItem onClick={goToProfile}>Your profile</UserMenuItem>
                    {canChangePassword && (
                        <UserMenuItem
                            onClick={() => {
                                setModal(() => <ChangePasswordModal />)
                            }}
                        >
                            Change your password
                        </UserMenuItem>
                    )}
                    <UserMenuItem onClick={signOut}>Sign out</UserMenuItem>
                </>
            ) : (
                <UserMenuItem>Not logged in currently.</UserMenuItem>
            )}
        </div>
    )
}

export default UserMenu
