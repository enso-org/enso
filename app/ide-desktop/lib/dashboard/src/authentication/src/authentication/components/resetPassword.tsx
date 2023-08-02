/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as validation from '../../dashboard/validation'

import Input from './input'
import SvgIcon from './svgIcon'
import SvgMask from './svgMask'

// =================
// === Constants ===
// =================

const RESET_PASSWORD_QUERY_PARAMS = {
    email: 'email',
    verificationCode: 'verification_code',
} as const

// =====================
// === ResetPassword ===
// =====================

/** A form for users to reset their password. */
export default function ResetPassword() {
    const { resetPassword } = auth.useAuth()
    const { search } = router.useLocation()

    const { verificationCode: initialCode, email: initialEmail } = parseUrlSearchParams(search)

    const [email, setEmail] = React.useState(initialEmail ?? '')
    const [code, setCode] = React.useState(initialCode ?? '')
    const [newPassword, setNewPassword] = React.useState('')
    const [newPasswordConfirm, setNewPasswordConfirm] = React.useState('')

    const onSubmit = () => {
        if (newPassword !== newPasswordConfirm) {
            toastify.toast.error('Passwords do not match')
            return Promise.resolve()
        } else {
            return resetPassword(email, code, newPassword)
        }
    }

    return (
        <div className="min-h-screen flex flex-col items-center justify-center">
            <div
                className={
                    'flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full ' +
                    'max-w-md'
                }
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Reset Your Password
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={async event => {
                            event.preventDefault()
                            await onSubmit()
                        }}
                    >
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="email"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                E-Mail Address:
                            </label>
                            <div className="relative">
                                <SvgIcon>
                                    <SvgMask src={AtIcon} />
                                </SvgIcon>
                                <Input
                                    id="email"
                                    type="email"
                                    name="email"
                                    placeholder="E-Mail Address"
                                    value={email}
                                    setValue={setEmail}
                                />
                            </div>
                        </div>
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="code"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                Confirmation Code:
                            </label>
                            <div className="relative">
                                <SvgIcon>
                                    <SvgMask src={LockIcon} />
                                </SvgIcon>
                                <Input
                                    id="code"
                                    type="text"
                                    name="code"
                                    placeholder="Confirmation Code"
                                    value={code}
                                    setValue={setCode}
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
                                    id="new_password"
                                    type="password"
                                    name="new_password"
                                    placeholder="New Password"
                                    pattern={validation.PASSWORD_PATTERN}
                                    title={validation.PASSWORD_ERROR}
                                    value={newPassword}
                                    setValue={setNewPassword}
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
                                    id="new_password_confirm"
                                    type="password"
                                    name="new_password_confirm"
                                    placeholder="Confirm New Password"
                                    value={newPasswordConfirm}
                                    setValue={setNewPasswordConfirm}
                                />
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className={
                                    'flex items-center justify-center focus:outline-none text-white text-sm ' +
                                    'sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition ' +
                                    'duration-150 ease-in'
                                }
                            >
                                <span className="mr-2 uppercase">Reset</span>
                                <span>
                                    <SvgMask src={ArrowRightIcon} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
                <div className="flex justify-center items-center mt-6">
                    <router.Link
                        to={app.LOGIN_PATH}
                        className={
                            'inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs ' +
                            'text-center'
                        }
                    >
                        <span>
                            <SvgMask src={GoBackIcon} />
                        </span>
                        <span className="ml-2">Go back to login</span>
                    </router.Link>
                </div>
            </div>
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const verificationCode = query.get(RESET_PASSWORD_QUERY_PARAMS.verificationCode)
    const email = query.get(RESET_PASSWORD_QUERY_PARAMS.email)
    return { verificationCode, email }
}
