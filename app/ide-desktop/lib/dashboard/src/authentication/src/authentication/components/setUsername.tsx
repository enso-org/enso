/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'

import * as auth from '../providers/auth'
import * as backendProvider from '../../providers/backend'
import * as svg from '../../components/svg'

import Dialog from './dialog'
import Input from './input'
import SubmitButton from './submitButton'
import SvgIcon from './svgIcon'

// ===================
// === SetUsername ===
// ===================

/** A form for users to set their username upon registration. */
function SetUsername() {
    const { setUsername: authSetUsername } = auth.useAuth()
    const { email } = auth.usePartialUserSession()
    const { backend } = backendProvider.useBackend()

    const [username, setUsername] = React.useState('')

    return (
        <Dialog
            title="Set your username"
            onSubmit={async event => {
                event.preventDefault()
                await authSetUsername(backend, username, email)
            }}
        >
            <div className="flex flex-col">
                <div className="relative">
                    <SvgIcon>
                        <svg.SvgMask src={AtIcon} />
                    </SvgIcon>
                    <Input
                        id="username"
                        type="text"
                        name="username"
                        placeholder="Username"
                        value={username}
                        setValue={setUsername}
                    />
                </div>
            </div>
            {/* Padding. */}
            <div />
            <SubmitButton>
                Set username
                <svg.SvgMask src={ArrowRightIcon} />
            </SubmitButton>
        </Dialog>
    )
}

export default SetUsername
