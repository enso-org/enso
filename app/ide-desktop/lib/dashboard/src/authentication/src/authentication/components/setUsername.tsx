/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'

import * as auth from '../providers/auth'
import * as backendProvider from '../../providers/backend'

import Input from './input'
import SubmitButton from './submitButton'

// ===================
// === SetUsername ===
// ===================

/** A form for users to set their username upon registration. */
export default function SetUsername() {
    const { setUsername: authSetUsername } = auth.useAuth()
    const { email } = auth.usePartialUserSession()
    const { backend } = backendProvider.useBackend()

    const [username, setUsername] = React.useState('')

    return (
        <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
            <form
                data-testid="set-username-panel"
                className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md"
                onSubmit={async event => {
                    event.preventDefault()
                    await authSetUsername(backend, username, email)
                }}
            >
                <div className="font-medium self-center text-xl">Set your username</div>
                <Input
                    id="username"
                    type="text"
                    name="username"
                    autoComplete="off"
                    label={null}
                    icon={AtIcon}
                    placeholder="Enter your username"
                    value={username}
                    setValue={setUsername}
                />
                <SubmitButton text="Set username" icon={ArrowRightIcon} />
            </form>
        </div>
    )
}
