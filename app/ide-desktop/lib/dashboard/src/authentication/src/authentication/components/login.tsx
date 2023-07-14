/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as svg from '../../components/svg'

import Dialog from './dialog'
import FontAwesomeIcon from './fontAwesomeIcon'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

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
function Login() {
    const { search } = router.useLocation()
    const { signInWithGoogle, signInWithGitHub, signInWithPassword } = auth.useAuth()

    const initialEmail = parseUrlSearchParams(search)

    const [email, setEmail] = React.useState(initialEmail ?? '')
    const [password, setPassword] = React.useState('')

    return (
        <Dialog
            title="Login"
            onSubmit={async event => {
                event.preventDefault()
                await signInWithPassword(email, password)
            }}
            header={
                <>
                    <button
                        onClick={async event => {
                            event.preventDefault()
                            await signInWithGoogle()
                        }}
                        className="relative rounded-full py-2 bg-gray-100 hover:bg-gray-200"
                    >
                        <FontAwesomeIcon icon={fontawesomeIcons.faGoogle} />
                        Login with Google
                    </button>
                    <button
                        onClick={async event => {
                            event.preventDefault()
                            await signInWithGitHub()
                        }}
                        className="relative rounded-full py-2 bg-gray-100 hover:bg-gray-200"
                    >
                        <FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
                        Login with Github
                    </button>
                    <div className="relative mt-4 h-px bg-gray-300">
                        <div className="absolute left-0 top-0 flex justify-center w-full -mt-2">
                            <span className="bg-primary-bg px-4 text-xs text-gray-500">
                                or login with email
                            </span>
                        </div>
                    </div>
                </>
            }
            footer={
                <div className="flex justify-center items-center">
                    <router.Link
                        to={app.REGISTRATION_PATH}
                        className="inline-flex items-center font-bold gap-2 text-blue-500 hover:text-blue-700 text-center"
                    >
                        <svg.SvgMask src={CreateAccountIcon} />
                        You don&apos;t have an account?
                    </router.Link>
                </div>
            }
        >
            <div className="flex flex-col gap-1">
                <label htmlFor="login_email">Email address</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={AtIcon} />
                    </SvgIcon>
                    <Input
                        required
                        id="login_email"
                        type="email"
                        name="login_email"
                        placeholder="Email address"
                        value={email}
                        setValue={setEmail}
                    />
                </div>
            </div>
            <div className="flex flex-col gap-1">
                <label htmlFor="login_password">Password</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={LockIcon} />
                    </SvgIcon>
                    <Input
                        required={true}
                        id="login_password"
                        type="password"
                        name="login_password"
                        placeholder="Password"
                        value={password}
                        setValue={setPassword}
                    />
                </div>
            </div>
            <div className="self-end -mt-2">
                <router.Link
                    to={app.FORGOT_PASSWORD_PATH}
                    className="text-xs sm:text-sm text-blue-500 hover:text-blue-700"
                >
                    Forgot Your Password?
                </router.Link>
            </div>
            <SubmitButton>
                Login
                <svg.SvgMask src={ArrowRightIcon} />
            </SubmitButton>
        </Dialog>
    )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const email = query.get(LOGIN_QUERY_PARAMS.email)
    return email
}

export default Login
