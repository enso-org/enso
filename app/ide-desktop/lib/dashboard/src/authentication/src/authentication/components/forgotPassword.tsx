/** @file Container responsible for rendering and interactions in first half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import GoBackIcon from 'enso-assets/go_back.svg'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import SvgMask from './svgMask'

import Input from './input'
import SvgIcon from './svgIcon'

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
    const { forgotPassword } = auth.useAuth()

    const [email, setEmail] = React.useState('')

    return (
        <div className="flex min-h-screen flex-col items-center justify-center">
            <div className="flex w-full max-w-md flex-col rounded-md bg-white p-8 shadow-md">
                <div className="self-center text-xl font-medium uppercase text-gray-800">
                    Forgot Your Password?
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={async event => {
                            event.preventDefault()
                            await forgotPassword(email)
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
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className="flex w-full items-center justify-center rounded bg-blue-600 py-2 text-sm text-white transition duration-150 ease-in hover:bg-blue-700 focus:outline-none"
                            >
                                <span className="mr-2 uppercase">Send link</span>
                                <span>
                                    <SvgMask src={ArrowRightIcon} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
                <div className="mt-6 flex items-center justify-center">
                    <router.Link
                        to={app.LOGIN_PATH}
                        className="inline-flex items-center text-center text-xs font-bold text-blue-500 hover:text-blue-700"
                    >
                        <span>
                            <SvgMask src={GoBackIcon} />
                        </span>
                        <span className="ml-2">Go back to login</span>
                    </router.Link>
                </div>
            </div>
        </div>
    )
}
