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
import * as string from '../../string'
import * as validation from '../../dashboard/validation'

import Input from './input'
import SubmitButton from './submitButton'
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
        <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
            <form
                className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md"
                onSubmit={async event => {
                    event.preventDefault()
                    await onSubmit()
                }}
            >
                <div className="font-medium self-center text-xl">Reset Your Password</div>
                <label className="flex flex-col gap-1">
                    Email:
                    <div className="relative">
                        <SvgIcon src={AtIcon} />
                        <Input
                            required
                            type="email"
                            autoComplete="email"
                            placeholder="Enter your email"
                            value={email}
                            setValue={setEmail}
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    Confirmation code:
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            type="text"
                            autoComplete="one-time-code"
                            placeholder="Enter the confirmation code"
                            value={code}
                            setValue={setCode}
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    New Password:
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Enter your new password"
                            pattern={validation.PASSWORD_PATTERN}
                            error={validation.PASSWORD_ERROR}
                            value={newPassword}
                            setValue={setNewPassword}
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    Confirm New Password:
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Confirm your new password"
                            pattern={string.regexEscape(newPassword)}
                            error={validation.CONFIRM_PASSWORD_ERROR}
                            value={newPasswordConfirm}
                            setValue={setNewPasswordConfirm}
                        />
                    </div>
                </label>
                <SubmitButton text="Reset" icon={ArrowRightIcon} />
            </form>
            <router.Link
                to={app.LOGIN_PATH}
                className="flex gap-2 items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
            >
                <SvgMask src={GoBackIcon} />
                Go back to login
            </router.Link>
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
