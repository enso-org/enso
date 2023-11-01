/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as authModule from '../providers/auth'
import * as string from '../../string'
import * as validation from '../../dashboard/validation'

import * as app from '../../components/app'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'
import SvgMask from './svgMask'

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
export default function Registration() {
    const auth = authModule.useAuth()
    const location = router.useLocation()
    const [email, setEmail] = React.useState('')
    const [password, setPassword] = React.useState('')
    const [confirmPassword, setConfirmPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)
    const { organizationId } = parseUrlSearchParams(location.search)

    return (
        <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
            <form
                className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md"
                onSubmit={async event => {
                    event.preventDefault()
                    setIsSubmitting(true)
                    await auth.signUp(email, password, organizationId)
                    setIsSubmitting(false)
                }}
            >
                <div className="font-medium self-center text-xl">Create a new account</div>
                <label className="flex flex-col gap-1">
                    Email
                    <div className="relative">
                        <SvgIcon src={AtIcon} />
                        <Input
                            required
                            validate
                            type="email"
                            autoComplete="email"
                            placeholder="Enter your email"
                            value={email}
                            setValue={setEmail}
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    Password
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Enter your password"
                            pattern={validation.PASSWORD_PATTERN}
                            error={validation.PASSWORD_ERROR}
                            value={password}
                            setValue={setPassword}
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    Confirm password
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Confirm your password"
                            pattern={string.regexEscape(password)}
                            error={validation.CONFIRM_PASSWORD_ERROR}
                            value={confirmPassword}
                            setValue={setConfirmPassword}
                        />
                    </div>
                </label>
                <SubmitButton disabled={isSubmitting} text="Register" icon={CreateAccountIcon} />
            </form>
            <router.Link
                to={app.LOGIN_PATH}
                className="flex gap-2 items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
            >
                <SvgMask src={GoBackIcon} />
                Already have an account?
            </router.Link>
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const organizationId = query.get(REGISTRATION_QUERY_PARAMS.organizationId)
    return { organizationId }
}
