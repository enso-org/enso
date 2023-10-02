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
        <div className="flex flex-col items-center justify-center min-h-screen p-8">
            <div
                className={
                    'rounded-md bg-white w-full max-w-sm sm:max-w-md border border-gray-200 ' +
                    'shadow-md px-4 py-6 sm:p-8'
                }
            >
                <div className="font-medium self-center text-xl uppercase text-gray-800">
                    Create new account
                </div>

                <form
                    onSubmit={async event => {
                        event.preventDefault()
                        setIsSubmitting(true)
                        await auth.signUp(email, password, organizationId)
                        setIsSubmitting(false)
                    }}
                >
                    <div className="flex flex-col mb-4">
                        <label htmlFor="email" className="mb-1 text-xs tracking-wide text-gray-600">
                            E-Mail Address:
                        </label>
                        <div className="relative">
                            <SvgIcon>
                                <SvgMask src={AtIcon} />
                            </SvgIcon>
                            <Input
                                required
                                validate
                                id="email"
                                type="email"
                                name="email"
                                autoComplete="email"
                                placeholder="E-Mail Address"
                                value={email}
                                setValue={setEmail}
                            />
                        </div>
                    </div>
                    <div className="flex flex-col mb-4">
                        <label
                            htmlFor="password"
                            className="mb-1 text-xs tracking-wide text-gray-600"
                        >
                            Password:
                        </label>
                        <div className="relative">
                            <SvgIcon>
                                <SvgMask src={LockIcon} />
                            </SvgIcon>
                            <Input
                                required
                                validate
                                id="password"
                                type="password"
                                name="password"
                                autoComplete="new-password"
                                placeholder="Password"
                                pattern={validation.PASSWORD_PATTERN}
                                error={validation.PASSWORD_ERROR}
                                value={password}
                                setValue={setPassword}
                            />
                        </div>
                    </div>
                    <div className="flex flex-col mb-4">
                        <label
                            htmlFor="password_confirmation"
                            className="mb-1 text-xs tracking-wide text-gray-600"
                        >
                            Confirm Password:
                        </label>
                        <div className="relative">
                            <SvgIcon>
                                <SvgMask src={LockIcon} />
                            </SvgIcon>
                            <Input
                                required
                                validate
                                id="password_confirmation"
                                type="password"
                                name="password_confirmation"
                                autoComplete="new-password"
                                placeholder="Confirm Password"
                                pattern={string.regexEscape(password)}
                                error={validation.CONFIRM_PASSWORD_ERROR}
                                value={confirmPassword}
                                setValue={setConfirmPassword}
                            />
                        </div>
                    </div>
                    <div className="flex w-full mt-6">
                        <button
                            disabled={isSubmitting}
                            type="submit"
                            className={
                                'flex items-center justify-center focus:outline-none text-white text-sm ' +
                                'bg-indigo-600 hover:bg-indigo-700 rounded py-2 w-full transition ' +
                                'duration-150 ease-in disabled:opacity-50'
                            }
                        >
                            <span className="mr-2 uppercase">Register</span>
                            <span>
                                <SvgMask src={CreateAccountIcon} />
                            </span>
                        </button>
                    </div>
                </form>
            </div>
            <div className="flex justify-center items-center mt-6">
                <router.Link
                    to={app.LOGIN_PATH}
                    className={
                        'inline-flex items-center font-bold text-indigo-500 hover:text-indigo-700 ' +
                        'text-sm text-center'
                    }
                >
                    <span>
                        <SvgMask src={GoBackIcon} />
                    </span>
                    <span className="ml-2">Already have an account?</span>
                </router.Link>
            </div>
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const organizationId = query.get(REGISTRATION_QUERY_PARAMS.organizationId)
    return { organizationId }
}
