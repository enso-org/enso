/** @file Container responsible for rendering and interactions in first half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import GoBackIcon from 'enso-assets/go_back.svg'

import * as app from '../../components/app'
import * as auth from '../providers/auth'
import * as svg from '../../components/svg'

import Dialog from './dialog'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
function ForgotPassword() {
    const { forgotPassword } = auth.useAuth()

    const [email, setEmail] = React.useState('')

    return (
        <Dialog
            title="Forgot your password?"
            onSubmit={async event => {
                event.preventDefault()
                await forgotPassword(email)
            }}
            footer={
                <div className="flex justify-center items-center">
                    <router.Link
                        to={app.LOGIN_PATH}
                        className={
                            'inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs ' +
                            'text-center'
                        }
                    >
                        <span>
                            <svg.SvgMask src={GoBackIcon} />
                        </span>
                        <span className="ml-2">Go back to login</span>
                    </router.Link>
                </div>
            }
        >
            <div className="flex flex-col gap-1">
                <label htmlFor="email">Email address</label>
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={AtIcon} />
                    </SvgIcon>
                    <Input
                        id="email"
                        type="email"
                        name="email"
                        placeholder="Email address"
                        value={email}
                        setValue={setEmail}
                    />
                </div>
            </div>
            {/* Padding. */}
            <div />
            <SubmitButton>
                Send link
                <svg.SvgMask src={ArrowRightIcon} />
            </SubmitButton>
        </Dialog>
    )
}

export default ForgotPassword
