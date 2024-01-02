/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'

import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'
import * as authProvider from '#/providers/AuthProvider'
import * as validation from '#/utilities/validation'

import FontAwesomeIcon from '#/components/FontAwesomeIcon'
import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'

// =================
// === Constants ===
// =================

const LOGIN_QUERY_PARAMS = {
    email: 'email',
} as const

// =============
// === Login ===
// =============

/** Props for a {@link Login}. */
export interface LoginProps {
    supportsLocalBackend: boolean
}

/** A form for users to log in. */
export default function Login(props: LoginProps) {
    const { supportsLocalBackend } = props
    const { search } = router.useLocation()
    const { signInWithGoogle, signInWithGitHub, signInWithPassword } = authProvider.useAuth()

    const initialEmail = parseUrlSearchParams(search)

    const [email, setEmail] = React.useState(initialEmail ?? '')
    const [password, setPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)
    const shouldReportValidityRef = React.useRef(true)

    return (
        <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
            <div className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md">
                <div className="font-medium self-center text-xl">Login to your account</div>
                <div className="flex flex-col gap-6">
                    <button
                        onMouseDown={() => {
                            shouldReportValidityRef.current = false
                        }}
                        onClick={async event => {
                            event.preventDefault()
                            await signInWithGoogle()
                        }}
                        className="relative rounded-full bg-cloud/10 hover:bg-cloud/20 focus:bg-cloud/20 transition-all duration-300 py-2"
                    >
                        <FontAwesomeIcon icon={fontawesomeIcons.faGoogle} />
                        Sign up or login with Google
                    </button>
                    <button
                        onMouseDown={() => {
                            shouldReportValidityRef.current = false
                        }}
                        onClick={async event => {
                            event.preventDefault()
                            await signInWithGitHub()
                        }}
                        className="relative rounded-full bg-cloud/10 hover:bg-cloud/20 focus:bg-cloud/20 transition-all duration-300 py-2"
                    >
                        <FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
                        Sign up or login with GitHub
                    </button>
                </div>
                <div className="flex items-center gap-2">
                    <div className="grow border-t border-primary/30 h-0" />
                    <span className="text-xs self-center text-primary/60">or login with email</span>
                    <div className="grow border-t border-primary/30 h-0" />
                </div>
                <form
                    className="flex flex-col gap-6"
                    onSubmit={async event => {
                        event.preventDefault()
                        setIsSubmitting(true)
                        await signInWithPassword(email, password)
                        shouldReportValidityRef.current = true
                        setIsSubmitting(false)
                    }}
                >
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
                        shouldReportValidityRef={shouldReportValidityRef}
                    />
                    <Input
                        required
                        validate
                        allowShowingPassword
                        type="password"
                        autoComplete="current-password"
                        label="Password"
                        icon={LockIcon}
                        placeholder="Enter your password"
                        error={validation.PASSWORD_ERROR}
                        value={password}
                        setValue={setPassword}
                        shouldReportValidityRef={shouldReportValidityRef}
                        footer={
                            <router.Link
                                to={appUtils.FORGOT_PASSWORD_PATH}
                                className="text-xs text-blue-500 hover:text-blue-700 focus:text-blue-700 transition-all duration-300 text-end"
                            >
                                Forgot Your Password?
                            </router.Link>
                        }
                    />
                    <SubmitButton disabled={isSubmitting} text="Login" icon={ArrowRightIcon} />
                </form>
            </div>
            <Link
                to={appUtils.REGISTRATION_PATH}
                icon={CreateAccountIcon}
                text="Don't have an account?"
            />
            {supportsLocalBackend && (
                <Link
                    to={appUtils.ENTER_OFFLINE_MODE_PATH}
                    icon={ArrowRightIcon}
                    text="Continue without creating an account"
                />
            )}
        </div>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const email = query.get(LOGIN_QUERY_PARAMS.email)
    return email
}
