/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as router from 'react-router-dom'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as common from './common'
import * as hooks from '../../hooks'
import * as icons from '../../components/svg'
import * as utils from '../../utils'

// =================
// === Constants ===
// =================

const BUTTON_CLASS_NAME =
    'relative mt-6 border rounded-md py-2 text-sm text-gray-800 ' + 'bg-gray-100 hover:bg-gray-200'

const LOGIN_QUERY_PARAMS = {
    email: 'email',
} as const

// =============
// === Login ===
// =============

function Login() {
    const { search } = router.useLocation()
    const { signInWithGoogle, signInWithGitHub, signInWithPassword } = auth.useAuth()

    const initialEmail = parseUrlSearchParams(search)

    const [email, bindEmail] = hooks.useInput(initialEmail ?? '')
    const [password, bindPassword] = hooks.useInput('')

    return (
        <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
            <div
                className={
                    'flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md ' +
                    'w-full max-w-md'
                }
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Login To Your Account
                </div>
                <button onClick={utils.handleEvent(signInWithGoogle)} className={BUTTON_CLASS_NAME}>
                    <common.FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
                    <span>Login with Google</span>
                </button>
                <button onClick={utils.handleEvent(signInWithGitHub)} className={BUTTON_CLASS_NAME}>
                    <common.FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
                    <span>Login with Github</span>
                </button>
                <div className="relative mt-10 h-px bg-gray-300">
                    <div className="absolute left-0 top-0 flex justify-center w-full -mt-2">
                        <span className="bg-white px-4 text-xs text-gray-500 uppercase">
                            Or Login With Email
                        </span>
                    </div>
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={utils.handleEvent(async () =>
                            signInWithPassword(email, password)
                        )}
                    >
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="email"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                E-Mail Address:
                            </label>
                            <div className="relative">
                                <common.SvgIcon data={icons.PATHS.at} />

                                <common.Input
                                    {...bindEmail}
                                    required={true}
                                    id="email"
                                    type="email"
                                    name="email"
                                    placeholder="E-Mail Address"
                                />
                            </div>
                        </div>
                        <div className="flex flex-col mb-6">
                            <label
                                htmlFor="password"
                                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                            >
                                Password:
                            </label>
                            <div className="relative">
                                <common.SvgIcon data={icons.PATHS.lock} />

                                <common.Input
                                    {...bindPassword}
                                    required={true}
                                    id="password"
                                    type="password"
                                    name="password"
                                    placeholder="Password"
                                />
                            </div>
                        </div>

                        <div className="flex items-center mb-6 -mt-4">
                            <div className="flex ml-auto">
                                <router.Link
                                    to={app.FORGOT_PASSWORD_PATH}
                                    className="inline-flex text-xs sm:text-sm text-blue-500 hover:text-blue-700"
                                >
                                    Forgot Your Password?
                                </router.Link>
                            </div>
                        </div>

                        <div className="flex w-full">
                            <button
                                type="submit"
                                className={
                                    'flex items-center justify-center focus:outline-none text-white ' +
                                    'text-sm sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full ' +
                                    'transition duration-150 ease-in'
                                }
                            >
                                <span className="mr-2 uppercase">Login</span>
                                <span>
                                    <icons.Svg data={icons.PATHS.rightArrow} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
                <div className="flex justify-center items-center mt-6">
                    <router.Link
                        to={app.REGISTRATION_PATH}
                        className={
                            'inline-flex items-center font-bold text-blue-500 hover:text-blue-700 ' +
                            'text-xs text-center'
                        }
                    >
                        <span>
                            <icons.Svg data={icons.PATHS.createAccount} />
                        </span>
                        <span className="ml-2">You don&apos;t have an account?</span>
                    </router.Link>
                </div>
            </div>
        </div>
    )
}

function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const email = query.get(LOGIN_QUERY_PARAMS.email)
    return email
}

export default Login
