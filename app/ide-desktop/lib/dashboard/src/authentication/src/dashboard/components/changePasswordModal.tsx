/** @file Managing the logic and displaying the UI for the password change function. */
import * as React from 'react'
import toast from 'react-hot-toast'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as auth from '../../authentication/providers/auth'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'
import * as validation from '../validation'

import Input from './input'
import Modal from './modal'
import SvgIcon from './svgIcon'

// ==========================
// === ResetPasswordModal ===
// ==========================

/** A modal for changing the user's password. */
function ChangePasswordModal() {
    const { changePassword } = auth.useAuth()
    const { unsetModal } = modalProvider.useSetModal()

    const [oldPassword, setOldPassword] = React.useState('')
    const [newPassword, setNewPassword] = React.useState('')
    const [confirmNewPassword, setConfirmNewPassword] = React.useState('')

    const onSubmit = async () => {
        if (newPassword !== confirmNewPassword) {
            toast.error('Passwords do not match.')
        } else {
            const success = await changePassword(oldPassword, newPassword)
            if (success) {
                unsetModal()
            }
        }
    }

    return (
        <Modal centered className="bg-opacity-30">
            <div
                onClick={event => {
                    event.stopPropagation()
                }}
                className="bg-primary-bg flex flex-col gap-4 shadow-soft p-4 rounded-2xl w-full max-w-sm"
            >
                <div className="font-semibold text-lg">Change your password</div>
                <form
                    onSubmit={async event => {
                        event.preventDefault()
                        await onSubmit()
                    }}
                    className="flex flex-col gap-3"
                >
                    <div className="flex flex-col gap-1">
                        <label htmlFor="old_password">Old password</label>
                        <div className="relative">
                            <SvgIcon>
                                <svg.SvgMask src={LockIcon} />
                            </SvgIcon>
                            <Input
                                autoFocus
                                required
                                id="old_password"
                                type="password"
                                name="old_password"
                                placeholder="Old Password"
                                pattern={validation.PREVIOUS_PASSWORD_PATTERN}
                                title={validation.PREVIOUS_PASSWORD_TITLE}
                                value={oldPassword}
                                setValue={setOldPassword}
                                className="bg-label placeholder-gray-500 pl-10 pr-4 rounded-2xl w-full py-2"
                            />
                        </div>
                    </div>
                    <div className="flex flex-col gap-1">
                        <label htmlFor="new_password">New password</label>
                        <div className="relative">
                            <SvgIcon>
                                <svg.SvgMask src={LockIcon} />
                            </SvgIcon>
                            <Input
                                required
                                id="new_password"
                                type="password"
                                name="new_password"
                                placeholder="New Password"
                                pattern={validation.PASSWORD_PATTERN}
                                title={validation.PASSWORD_TITLE}
                                value={newPassword}
                                setValue={setNewPassword}
                                className="bg-label placeholder-gray-500 pl-10 pr-4 rounded-2xl w-full py-2"
                            />
                        </div>
                    </div>
                    <div className="flex flex-col gap-1">
                        <label htmlFor="new_password_confirm">Confirm new password</label>
                        <div className="relative">
                            <SvgIcon>
                                <svg.SvgMask src={LockIcon} />
                            </SvgIcon>
                            <Input
                                required
                                id="confirm_new_password"
                                type="password"
                                name="confirm_new_password"
                                placeholder="Confirm New Password"
                                value={confirmNewPassword}
                                setValue={setConfirmNewPassword}
                                className="bg-label placeholder-gray-500 pl-10 pr-4 rounded-2xl w-full py-2"
                            />
                        </div>
                    </div>
                    <div className="flex w-full mt-2">
                        <button
                            type="submit"
                            className="flex items-center justify-center text-white bg-blue-600 hover:bg-blue-700 rounded-full gap-2 w-full py-2 transition duration-150 ease-in"
                        >
                            <span className="text-sm">Reset</span>
                            <span>
                                <svg.SvgMask src={ArrowRightIcon} />
                            </span>
                        </button>
                    </div>
                </form>
            </div>
        </Modal>
    )
}

export default ChangePasswordModal
