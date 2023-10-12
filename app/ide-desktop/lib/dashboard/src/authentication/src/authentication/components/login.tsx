/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as auth from '../providers/auth'
import * as validation from '../../dashboard/validation'

import * as app from '../../components/app'
import FontAwesomeIcon from './fontAwesomeIcon'
import Input from './input'
import SvgIcon from './svgIcon'
import SvgMask from './svgMask'

// =================
// === Constants ===
// =================

const LOGIN_QUERY_PARAMS = {
    email: 'email',
} as const

// =============
// === Login ===
// =============

/** A form for users to log in. */
export default function Login() {
    const { search } = router.useLocation()
    const { signInWithGoogle, signInWithGitHub, signInWithPassword } = auth.useAuth()

    const initialEmail = parseUrlSearchParams(search)

    const [email, setEmail] = React.useState(initialEmail ?? '')
    const [password, setPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)
    const shouldReportValidityRef = React.useRef(true)

    return (
        <div className="flex min-h-screen flex-col items-center justify-center">
            <div className="flex w-full max-w-md flex-col rounded-md bg-white p-8 shadow-md">
                <div className="self-center text-xl font-medium uppercase text-gray-800">
                    Login To Your Account
                </div>
                <button
                    onMouseDown={() => {
                        shouldReportValidityRef.current = false
                    }}
                    onClick={async event => {
                        event.preventDefault()
                        await signInWithGoogle()
                    }}
                    className="relative mt-6 rounded-md border bg-gray-100 py-2 text-sm text-gray-800 hover:bg-gray-200"
                >
                    <FontAwesomeIcon icon={fontawesomeIcons.faGoogle} />
                    <span>Sign Up or Login with Google</span>
                </button>
                <button
                    onMouseDown={() => {
                        shouldReportValidityRef.current = false
                    }}
                    onClick={async event => {
                        event.preventDefault()
                        await signInWithGitHub()
                    }}
                    className="relative mt-6 rounded-md border bg-gray-100 py-2 text-sm text-gray-800 hover:bg-gray-200"
                >
                    <FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
                    <span>Sign Up or Login with Github</span>
                </button>
                <div className="relative mt-10 h-px bg-gray-300">
                    <div className="absolute left-0 top-0 -mt-2 flex w-full justify-center">
                        <span className="bg-white px-4 text-xs uppercase text-gray-500">
                            Or Login With Email
                        </span>
                    </div>
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={async event => {
                            event.preventDefault()
                            setIsSubmitting(true)
                            await signInWithPassword(email, password)
                            shouldReportValidityRef.current = true
                            setIsSubmitting(false)
                        }}
                    >
                        <div className="mb-6 flex flex-col">
                            <label
                                htmlFor="email"
                                className="mb-1 text-xs tracking-wide text-gray-600"
                            >
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
                                    shouldReportValidityRef={shouldReportValidityRef}
                                />
                            </div>
                        </div>
                        <div className="mb-6 flex flex-col">
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
                                    autoComplete="current-password"
                                    placeholder="Password"
                                    pattern={validation.PASSWORD_PATTERN}
                                    error={validation.PASSWORD_ERROR}
                                    value={password}
                                    setValue={setPassword}
                                    shouldReportValidityRef={shouldReportValidityRef}
                                />
                            </div>
                        </div>
                        <div className="-mt-4 mb-6 flex items-center">
                            <div className="ml-auto flex">
                                <router.Link
                                    to={app.FORGOT_PASSWORD_PATH}
                                    className="inline-flex text-xs text-blue-500 hover:text-blue-700"
                                >
                                    Forgot Your Password?
                                </router.Link>
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                disabled={isSubmitting}
                                type="submit"
                                className="flex w-full items-center justify-center rounded bg-blue-600 py-2 text-sm text-white transition duration-150 ease-in hover:bg-blue-700 focus:outline-none disabled:opacity-50"
                            >
                                <span className="mr-2 uppercase">Login</span>
                                <SvgMask src={ArrowRightIcon} />
                            </button>
                        </div>
                    </form>
                </div>
                <div className="mt-6 flex items-center justify-center">
                    <router.Link
                        to={app.REGISTRATION_PATH}
                        className="inline-flex items-center text-center text-xs font-bold text-blue-500 hover:text-blue-700"
                    >
                        <SvgMask src={CreateAccountIcon} />
                        <span className="ml-2">You don&apos;t have an account?</span>
                    </router.Link>
                </div>
                <div className="mt-6 flex items-center justify-center">
                    <router.Link
                        to={app.ENTER_OFFLINE_MODE_PATH}
                        className="inline-flex items-center text-center text-xs font-bold text-blue-500 hover:text-blue-700"
                    >
                        <SvgMask src={ArrowRightIcon} />
                        <span className="ml-2">Continue without creating an account</span>
                    </router.Link>
                </div>
            </div>
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const email = query.get(LOGIN_QUERY_PARAMS.email)
    return email
}
