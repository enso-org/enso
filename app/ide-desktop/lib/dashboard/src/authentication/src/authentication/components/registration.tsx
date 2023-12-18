/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as authModule from '../providers/auth'
import * as localStorageModule from '../../dashboard/localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as string from '../../string'
import * as validation from '../../dashboard/validation'

import * as app from '../../components/app'
import Input from './input'
import Link from './link'
import SubmitButton from './submitButton'

// =================
// === Constants ===
// =================

const REGISTRATION_QUERY_PARAMS = {
    email: 'email',
    organizationId: 'organization_id',
    redirectTo: 'redirect_to',
} as const

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
    const auth = authModule.useAuth()
    const location = router.useLocation()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { email: urlEmail, organizationId, redirectTo } = parseUrlSearchParams(location.search)
    const [email, setEmail] = React.useState(urlEmail ?? '')
    const [password, setPassword] = React.useState('')
    const [confirmPassword, setConfirmPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)

    React.useEffect(() => {
        if (redirectTo != null) {
            localStorage.set(localStorageModule.LocalStorageKey.loginRedirect, redirectTo)
        } else {
            localStorage.delete(localStorageModule.LocalStorageKey.loginRedirect)
        }
    }, [localStorage, redirectTo])

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
                <Input
                    required
                    validate
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
                    validate
                    allowShowingPassword
                    type="password"
                    autoComplete="new-password"
                    label="Password"
                    icon={LockIcon}
                    placeholder="Enter your password"
                    pattern={validation.PASSWORD_PATTERN}
                    error={validation.PASSWORD_ERROR}
                    value={password}
                    setValue={setPassword}
                />
                <Input
                    required
                    validate
                    allowShowingPassword
                    type="password"
                    autoComplete="new-password"
                    label="Confirm password"
                    icon={LockIcon}
                    placeholder="Confirm your password"
                    pattern={string.regexEscape(password)}
                    error={validation.CONFIRM_PASSWORD_ERROR}
                    value={confirmPassword}
                    setValue={setConfirmPassword}
                />
                <SubmitButton disabled={isSubmitting} text="Register" icon={CreateAccountIcon} />
            </form>
            <Link to={app.LOGIN_PATH} icon={GoBackIcon} text="Already have an account?" />
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const email = query.get(REGISTRATION_QUERY_PARAMS.email)
    const organizationId = query.get(REGISTRATION_QUERY_PARAMS.organizationId)
    const redirectTo = query.get(REGISTRATION_QUERY_PARAMS.redirectTo)
    return { email, organizationId, redirectTo }
}
