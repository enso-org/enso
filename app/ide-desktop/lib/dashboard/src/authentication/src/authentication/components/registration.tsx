/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'
import * as router from 'react-router-dom'
import toast from 'react-hot-toast'

import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as app from '../../components/app'
import * as authModule from '../providers/auth'
import * as svg from '../../components/svg'
import * as validation from '../../dashboard/validation'

import Dialog from './dialog'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

// =================
// === Constants ===
// =================

const REGISTRATION_QUERY_PARAMS = {
    organizationId: 'organization_id',
} as const

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
function Registration() {
    const auth = authModule.useAuth()
    const location = router.useLocation()
    const [email, setEmail] = React.useState('')
    const [password, setPassword] = React.useState('')
    const [confirmPassword, setConfirmPassword] = React.useState('')

    const { organizationId } = parseUrlSearchParams(location.search)

    const onSubmit = () => {
        /** The password & confirm password fields must match. */
        if (password !== confirmPassword) {
            toast.error('Passwords do not match.')
            return Promise.resolve()
        } else {
            return auth.signUp(email, password, organizationId)
        }
    }

    return (
        <Dialog
            title="Create new account"
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
                        Already have an account?
                    </router.Link>
                </div>
            }
        >
            <div className="flex flex-col gap-1">
                <label htmlFor="registration_email">Email address</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={AtIcon} />
                    </SvgIcon>
                    <Input
                        id="registration_email"
                        type="email"
                        name="registration_email"
                        placeholder="Email address"
                        value={email}
                        setValue={setEmail}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="registration_password">Password:</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        required
                        id="registration_password"
                        type="password"
                        name="registration_password"
                        placeholder="Password"
                        pattern={validation.PASSWORD_PATTERN}
                        title={validation.PASSWORD_TITLE}
                        value={password}
                        setValue={setPassword}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="password_confirmation">Confirm Password:</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        required
                        id="registration_password_confirmation"
                        type="password"
                        name="registration_password_confirmation"
                        placeholder="Confirm password"
                        value={confirmPassword}
                        setValue={setConfirmPassword}
                    />
                </div>
            </div>
            {/* Padding. */}
            <div />
            <SubmitButton>
                Register
                <svg.SvgMask src={CreateAccountIcon} />
            </SubmitButton>
        </Dialog>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const organizationId = query.get(REGISTRATION_QUERY_PARAMS.organizationId)
    return { organizationId }
}

export default Registration
