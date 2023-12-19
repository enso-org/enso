/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as hooks from '../../hooks'
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
    const navigate = hooks.useNavigate()

    const { verificationCode, email } = parseUrlSearchParams(search)

    const [newPassword, setNewPassword] = React.useState('')
    const [newPasswordConfirm, setNewPasswordConfirm] = React.useState('')

    React.useEffect(() => {
        if (email == null) {
            toastify.toast.error('Could not reset password: missing email address')
            navigate(app.LOGIN_PATH)
        } else if (verificationCode == null) {
            toastify.toast.error('Could not reset password: missing verification code')
            navigate(app.LOGIN_PATH)
        }
    }, [email, navigate, verificationCode])

    const onSubmit = () => {
        if (newPassword !== newPasswordConfirm) {
            toastify.toast.error('Passwords do not match')
            return Promise.resolve()
        } else {
            // These should never be nullish, as the effect should immediately navigate away.
            return resetPassword(email ?? '', verificationCode ?? '', newPassword)
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
                <input
                    required
                    readOnly
                    hidden
                    type="email"
                    autoComplete="email"
                    placeholder="Enter your email"
                    value={email ?? ''}
                />
                <input
                    required
                    readOnly
                    hidden
                    type="text"
                    autoComplete="one-time-code"
                    placeholder="Enter the confirmation code"
                    value={verificationCode ?? ''}
                />
                <Input
                    required
                    validate
                    allowShowingPassword
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
                    allowShowingPassword
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
