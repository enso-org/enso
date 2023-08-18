/** @file Managing the logic and displaying the UI for the password change function. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as auth from '../../authentication/providers/auth'
import * as modalProvider from '../../providers/modal'
import * as string from '../../string'
import * as validation from '../validation'

import Input from './input'
import Modal from './modal'
import SvgIcon from './svgIcon'
import SvgMask from '../../authentication/components/svgMask'

// ==========================
// === ResetPasswordModal ===
// ==========================

/** A modal for changing the user's password. */
export default function ChangePasswordModal() {
    const { changePassword } = auth.useAuth()
    const { unsetModal } = modalProvider.useSetModal()

    const [oldPassword, setOldPassword] = React.useState('')
    const [newPassword, setNewPassword] = React.useState('')
    const [confirmNewPassword, setConfirmNewPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)

    return (
        <Modal centered className="bg-dim">
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
                        onSubmit={async event => {
                            event.preventDefault()
                            setIsSubmitting(true)
                            const success = await changePassword(oldPassword, newPassword)
                            setIsSubmitting(false)
                            if (success) {
                                unsetModal()
                            }
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
                                <SvgIcon>
                                    <SvgMask src={LockIcon} />
                                </SvgIcon>
                                <Input
                                    autoFocus
                                    required
                                    validate
                                    id="old_password"
                                    type="password"
                                    name="old_password"
                                    placeholder="Old Password"
                                    pattern={validation.PASSWORD_PATTERN}
                                    error={validation.PASSWORD_ERROR}
                                    value={oldPassword}
                                    setValue={setOldPassword}
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
                                <SvgIcon>
                                    <SvgMask src={LockIcon} />
                                </SvgIcon>
                                <Input
                                    required
                                    validate
                                    id="new_password"
                                    type="password"
                                    name="new_password"
                                    placeholder="New Password"
                                    pattern={validation.PASSWORD_PATTERN}
                                    error={validation.PASSWORD_ERROR}
                                    value={newPassword}
                                    setValue={setNewPassword}
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
                                <SvgIcon>
                                    <SvgMask src={LockIcon} />
                                </SvgIcon>
                                <Input
                                    required
                                    validate
                                    id="confirm_new_password"
                                    type="password"
                                    name="confirm_new_password"
                                    placeholder="Confirm New Password"
                                    pattern={string.regexEscape(newPassword)}
                                    error={validation.CONFIRM_PASSWORD_ERROR}
                                    value={confirmNewPassword}
                                    setValue={setConfirmNewPassword}
                                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                                />
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                disabled={isSubmitting}
                                type="submit"
                                className="flex items-center justify-center focus:outline-none text-white text-sm sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition duration-150 ease-in disabled:opacity-50"
                            >
                                <span className="mr-2 uppercase">Reset</span>
                                <span>
                                    <SvgMask src={ArrowRightIcon} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
        </Modal>
    )
}
