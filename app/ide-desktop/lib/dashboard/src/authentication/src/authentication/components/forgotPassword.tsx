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
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
    const { forgotPassword } = auth.useAuth()

    const [email, setEmail] = React.useState('')

    return (
        <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
            <form
                className="flex flex-col gap-6 bg-frame-selected rounded-2xl shadow-md p-8 w-full max-w-md"
                onSubmit={async event => {
                    event.preventDefault()
                    await forgotPassword(email)
                }}
            >
                <div className="font-medium self-center text-xl">Forgot Your Password?</div>
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
                <SubmitButton text="Send link" icon={ArrowRightIcon} />
            </form>
            <router.Link
                to={app.LOGIN_PATH}
                className="flex gap-2 items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
            >
                <SvgMask src={GoBackIcon} />
                Go back to login
            </router.Link>
        </div>
    )
}
