/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as router from 'react-router-dom'
import toast from 'react-hot-toast'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as common from './common'
import * as hooks from '../../hooks'
import * as icons from '../../components/svg'
import * as utils from '../../utils'

// ====================
// === Registration ===
// ====================

function Registration() {
    const { signUp } = auth.useAuth()
    const [email, bindEmail] = hooks.useInput('')
    const [password, bindPassword] = hooks.useInput('')
    const [confirmPassword, bindConfirmPassword] = hooks.useInput('')

    const handleSubmit = () => {
        /** The password & confirm password fields must match. */
        if (password !== confirmPassword) {
            toast.error('Passwords do not match.')
            return Promise.resolve()
        } else {
            return signUp(email, password)
        }
    }

    return (
        <div className="flex flex-col items-center justify-center min-h-screen bg-gray-300 px-4 py-8">
            <div
                className={
                    'rounded-md bg-white w-full max-w-sm sm:max-w-md border border-gray-200 ' +
                    'shadow-md px-4 py-6 sm:p-8'
                }
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Create new account
                </div>

                <form onSubmit={utils.handleEvent(handleSubmit)}>
                    <div className="flex flex-col mb-4">
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
                                id="email"
                                type="email"
                                name="email"
                                placeholder="E-Mail Address"
                            />
                        </div>
                    </div>
                    <div className="flex flex-col mb-4">
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
                                id="password"
                                type="password"
                                name="password"
                                placeholder="Password"
                            />
                        </div>
                    </div>
                    <div className="flex flex-col mb-4">
                        <label
                            htmlFor="password_confirmation"
                            className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                        >
                            Confirm Password:
                        </label>
                        <div className="relative">
                            <common.SvgIcon data={icons.PATHS.lock} />

                            <common.Input
                                {...bindConfirmPassword}
                                id="password_confirmation"
                                type="password"
                                name="password_confirmation"
                                placeholder="Confirm Password"
                            />
                        </div>
                    </div>

                    <div className="flex w-full mt-6">
                        <button
                            type="submit"
                            className={
                                'flex items-center justify-center focus:outline-none text-white text-sm ' +
                                'sm:text-base bg-indigo-600 hover:bg-indigo-700 rounded py-2 w-full transition ' +
                                'duration-150 ease-in'
                            }
                        >
                            <span className="mr-2 uppercase">Register</span>
                            <span>
                                <icons.Svg data={icons.PATHS.createAccount} />
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
                        <icons.Svg data={icons.PATHS.goBack} />
                    </span>
                    <span className="ml-2">Already have an account?</span>
                </router.Link>
            </div>
        </div>
    )
}

export default Registration
