/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'

import * as auth from '../providers/auth'
import * as backendProvider from '../../providers/backend'
import SvgMask from './svgMask'

import Input from './input'
import SvgIcon from './svgIcon'

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
        <div className="flex min-h-screen flex-col items-center justify-center">
            <div
                data-testid="set-username-panel"
                className="flex w-full max-w-md flex-col rounded-md bg-white px-4 py-8 shadow-md sm:px-6 md:px-8 lg:px-10"
            >
                <div className="self-center text-xl font-medium uppercase text-gray-800">
                    Set your username
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={async event => {
                            event.preventDefault()
                            await authSetUsername(backend, username, email)
                        }}
                    >
                        <div className="mb-6 flex flex-col">
                            <div className="relative">
                                <SvgIcon>
                                    <SvgMask src={AtIcon} />
                                </SvgIcon>

                                <Input
                                    id="username"
                                    type="text"
                                    name="username"
                                    autoComplete="off"
                                    placeholder="Username"
                                    value={username}
                                    setValue={setUsername}
                                />
                            </div>
                        </div>
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className="flex w-full items-center justify-center rounded bg-blue-600 py-2 text-sm text-white transition duration-150 ease-in hover:bg-blue-700 focus:outline-none"
                            >
                                <span className="mr-2 uppercase">Set username</span>
                                <span>
                                    <SvgMask src={ArrowRightIcon} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    )
}
