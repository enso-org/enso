/** @file Managing the logic and displaying the UI for the password change function. */
import * as react from 'react'

import toast from 'react-hot-toast'

import * as auth from '../../authentication/providers/auth'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Modal from './modal'

// ==========================
// === ResetPasswordModal ===
// ==========================

function ChangePasswordModal() {
    const { changePassword } = auth.useAuth()
    const { unsetModal } = modalProvider.useSetModal()

    const [oldPassword, setOldPassword] = react.useState('')
    const [newPassword, setNewPassword] = react.useState('')
    const [confirmNewPassword, setConfirmNewPassword] = react.useState('')

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
        <Modal className="bg-opacity-30">
            <div
                onClick={event => {
                    event.stopPropagation()
                }}
                className="flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full max-w-md"
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Change Your Password
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={event => {
                            event.preventDefault()
                            void onSubmit()
                        }}
                    >
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="old_password"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                Old Password:
                            </label>
                            <div className="relative">
                                <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                                    {svg.LOCK}
                                </div>

                                <input
                                    id="old_password"
                                    type="password"
                                    name="old_password"
                                    placeholder="Old Password"
                                    value={oldPassword}
                                    onChange={event => {
                                        setOldPassword(event.target.value)
                                    }}
                                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                                />
                            </div>
                        </div>
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="new_password"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                New Password:
                            </label>
                            <div className="relative">
                                <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                                    {svg.LOCK}
                                </div>

                                <input
                                    id="new_password"
                                    type="password"
                                    name="new_password"
                                    placeholder="New Password"
                                    value={newPassword}
                                    onChange={event => {
                                        setNewPassword(event.target.value)
                                    }}
                                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                                />
                            </div>
                        </div>
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="new_password_confirm"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                Confirm New Password:
                            </label>
                            <div className="relative">
                                <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                                    {svg.LOCK}
                                </div>

                                <input
                                    id="confirm_new_password"
                                    type="password"
                                    name="confirm_new_password"
                                    placeholder="Confirm New Password"
                                    value={confirmNewPassword}
                                    onChange={event => {
                                        setConfirmNewPassword(event.target.value)
                                    }}
                                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                                />
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className="flex items-center justify-center focus:outline-none text-white text-sm sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition duration-150 ease-in"
                            >
                                <span className="mr-2 uppercase">Reset</span>
                                <span>{svg.RIGHT_ARROW}</span>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
        </Modal>
    )
}

export default ChangePasswordModal
