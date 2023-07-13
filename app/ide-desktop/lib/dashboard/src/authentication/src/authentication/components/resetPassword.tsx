/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'
import toast from 'react-hot-toast'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as svg from '../../components/svg'
import * as validation from '../../dashboard/validation'

import Dialog from './dialog'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

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
function ResetPassword() {
    const { resetPassword } = auth.useAuth()
    const { search } = router.useLocation()

    const { verificationCode: initialCode, email: initialEmail } = parseUrlSearchParams(search)

    const [email, setEmail] = React.useState(initialEmail ?? '')
    const [code, setCode] = React.useState(initialCode ?? '')
    const [newPassword, setNewPassword] = React.useState('')
    const [newPasswordConfirm, setNewPasswordConfirm] = React.useState('')

    const onSubmit = () => {
        if (newPassword !== newPasswordConfirm) {
            toast.error('Passwords do not match')
            return Promise.resolve()
        } else {
            return resetPassword(email, code, newPassword)
        }
    }

    return (
        <Dialog
            title="Reset your password"
            onSubmit={async event => {
                event.preventDefault()
                await onSubmit()
            }}
            footer={
                <div className="flex justify-center items-center">
                    <router.Link
                        to={app.LOGIN_PATH}
                        className="inline-flex items-center font-bold gap-2 text-blue-500 hover:text-blue-700 text-center"
                    >
                        <svg.SvgMask src={GoBackIcon} />
                        Go back to login
                    </router.Link>
                </div>
            }
        >
            <div className="flex flex-col gap-1">
                <label htmlFor="reset_password_email">Email address</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={AtIcon} />
                    </SvgIcon>
                    <Input
                        id="reset_password_email"
                        type="email"
                        name="reset_password_email"
                        placeholder="Email address"
                        value={email}
                        setValue={setEmail}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="reset_password_code">Confirmation Code:</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        id="reset_password_code"
                        type="text"
                        name="reset_password_code"
                        placeholder="Confirmation Code"
                        value={code}
                        setValue={setCode}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="reset_password_new_password">New Password:</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        id="reset_password_new_password"
                        type="password"
                        name="reset_password_new_password"
                        placeholder="New Password"
                        pattern={validation.PASSWORD_PATTERN}
                        title={validation.PASSWORD_TITLE}
                        value={newPassword}
                        setValue={setNewPassword}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="reset_password_new_password_confirm">Confirm new password</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        id="reset_password_new_password_confirm"
                        type="password"
                        name="reset_password_new_password_confirm"
                        placeholder="Confirm New Password"
                        value={newPasswordConfirm}
                        setValue={setNewPasswordConfirm}
                    />
                </div>
            </div>
            {/* Padding. */}
            <div />
            <SubmitButton>
                Reset
                <svg.SvgMask src={ArrowRightIcon} />
            </SubmitButton>
        </Dialog>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const verificationCode = query.get(RESET_PASSWORD_QUERY_PARAMS.verificationCode)
    const email = query.get(RESET_PASSWORD_QUERY_PARAMS.email)
    return { verificationCode, email }
}

export default ResetPassword
