/** @file Managing the logic and displaying the UI for the password change function. */
import toast from 'react-hot-toast'

import * as auth from '../../authentication/providers/auth'
import * as hooks from '../../hooks'
import * as icons from '../../components/svg'
import * as utils from '../../utils'
import Modal from './modal'

interface ChangePasswordModalProps {
    visible: boolean
    handleCancel: () => void
}

// ==========================
// === ResetPasswordModal ===
// ==========================

function ChangePasswordModal(props: ChangePasswordModalProps) {
    const { visible, handleCancel } = props
    const { changePassword } = auth.useAuth()

    const [oldPassword, bindOldPassword] = hooks.useInput('')
    const [newPassword, bindNewPassword] = hooks.useInput('')
    const [newPasswordConfirm, bindNewPasswordConfirm] = hooks.useInput('')
    const handleSubmit = async () => {
        if (newPassword !== newPasswordConfirm) {
            toast.error('Passwords do not match.')
        } else {
            await changePassword(oldPassword, newPassword)
        }
    }

    return (
        <Modal visible={visible} onCancel={handleCancel}>
            <div
                className={`flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 
                            py-8 rounded-md w-full max-w-md`}
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Change Your Password
                </div>
                <div className="mt-10">
                    <form onSubmit={utils.handleEvent(handleSubmit)}>
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="old_password"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                Old Password:
                            </label>
                            <div className="relative">
                                <div
                                    className={`inline-flex items-center justify-center absolute 
                                                left-0 top-0 h-full w-10 text-gray-400`}
                                >
                                    <icons.Svg data={icons.PATHS.lock} />
                                </div>

                                <input
                                    {...bindOldPassword}
                                    id="old_password"
                                    type="password"
                                    name="old_password"
                                    className={`text-sm sm:text-base placeholder-gray-500 pl-10 
                                                pr-4 rounded-lg border border-gray-400 w-full 
                                                py-2 focus:outline-none focus:border-blue-400`}
                                    placeholder="Old Password"
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
                                <div
                                    className={`inline-flex items-center justify-center absolute
                                                left-0 top-0 h-full w-10 text-gray-400`}
                                >
                                    <icons.Svg data={icons.PATHS.lock} />
                                </div>

                                <input
                                    {...bindNewPassword}
                                    id="new_password"
                                    type="password"
                                    name="new_password"
                                    className={`text-sm sm:text-base placeholder-gray-500 pl-10 
                                                pr-4 rounded-lg border border-gray-400 w-full 
                                                py-2 focus:outline-none focus:border-blue-400`}
                                    placeholder="New Password"
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
                                <div
                                    className={`inline-flex items-center justify-center absolute 
                                                left-0 top-0 h-full w-10 text-gray-400`}
                                >
                                    <icons.Svg data={icons.PATHS.lock} />
                                </div>

                                <input
                                    {...bindNewPasswordConfirm}
                                    id="new_password_confirm"
                                    type="password"
                                    name="new_password_confirm"
                                    className={`text-sm sm:text-base placeholder-gray-500 pl-10 
                                                pr-4 rounded-lg border border-gray-400 w-full 
                                                py-2 focus:outline-none focus:border-blue-400`}
                                    placeholder="Confirm New Password"
                                />
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className={`flex items-center justify-center focus:outline-none 
                                            text-white text-sm sm:text-base bg-blue-600 
                                            hover:bg-blue-700 rounded py-2 w-full transition 
                                            duration-150 ease-in`}
                            >
                                <span className="mr-2 uppercase">Reset</span>
                                <span>
                                    <icons.Svg data={icons.PATHS.rightArrow} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
        </Modal>
    )
}

export default ChangePasswordModal
