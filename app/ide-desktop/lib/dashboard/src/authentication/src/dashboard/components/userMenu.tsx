/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as react from 'react'

import * as auth from '../../authentication/providers/auth'
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
    const { organization } = auth.useFullUserSession()

    const [visibleChangePassword, setVisibleChangePassword] = react.useState(false)

    const goToProfile = () => {
        // TODO: Implement this when the backend endpoints are implemented.
    }

    const showChangePasswordModal = () => {
        setVisibleChangePassword(true)
    }
    const hideChangePasswordModal = () => {
        setVisibleChangePassword(false)
    }

    return (
        <>
            <div className="absolute right-0 top-9 flex flex-col rounded-md bg-white py-1 border">
                {organization ? (
                    <>
                        <UserMenuItem>
                            Signed in as <span className="font-bold">{organization.name}</span>
                        </UserMenuItem>
                        <UserMenuItem onClick={goToProfile}>Your profile</UserMenuItem>
                        <UserMenuItem onClick={showChangePasswordModal}>
                            Change your password
                        </UserMenuItem>
                        <UserMenuItem onClick={signOut}>Sign out</UserMenuItem>
                    </>
                ) : (
                    <>
                        <UserMenuItem>Not logged in currently.</UserMenuItem>
                    </>
                )}
            </div>
            <ChangePasswordModal
                visible={visibleChangePassword}
                handleCancel={hideChangePasswordModal}
            />
        </>
    )
}

export default UserMenu
