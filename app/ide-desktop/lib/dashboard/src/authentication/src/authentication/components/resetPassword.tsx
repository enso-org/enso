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
import Link from './link'
import SubmitButton from './submitButton'

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
                <div className="font-medium self-center text-xl">Reset your password</div>
                <Input
                    required
                    type="email"
                    autoComplete="email"
                    label="Email"
                    icon={AtIcon}
                    placeholder="Enter your email"
                    value={email}
                    setValue={setEmail}
                />
                <Input
                    required
                    type="text"
                    autoComplete="one-time-code"
                    label="Confirmation code"
                    icon={LockIcon}
                    placeholder="Enter the confirmation code"
                    value={code}
                    setValue={setCode}
                />
                <Input
                    required
                    validate
                    type="password"
                    autoComplete="new-password"
                    label="New password"
                    icon={LockIcon}
                    placeholder="Enter your new password"
                    pattern={validation.PASSWORD_PATTERN}
                    error={validation.PASSWORD_ERROR}
                    value={newPassword}
                    setValue={setNewPassword}
                />
                <Input
                    required
                    validate
                    type="password"
                    autoComplete="new-password"
                    label="Confirm new password"
                    icon={LockIcon}
                    placeholder="Confirm your new password"
                    pattern={string.regexEscape(newPassword)}
                    error={validation.CONFIRM_PASSWORD_ERROR}
                    value={newPasswordConfirm}
                    setValue={setNewPasswordConfirm}
                />
                <SubmitButton text="Reset" icon={ArrowRightIcon} />
            </form>
            <Link to={app.LOGIN_PATH} icon={GoBackIcon} text="Go back to login" />
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
