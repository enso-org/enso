/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'

import * as auth from '../providers/auth'
import * as backendProvider from '../../providers/backend'
import * as svg from '../../components/svg'

import Input from './input'
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
        <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
            <div
                className={
                    'flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full ' +
                    'max-w-md'
                }
            >
                <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
                    Set your username
                </div>
                <div className="mt-10">
                    <form
                        onSubmit={async event => {
                            event.preventDefault()
                            await authSetUsername(backend, username, email)
                        }}
                    >
                        <div className="flex flex-col mb-6">
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
                        <div className="flex w-full">
                            <button
                                type="submit"
                                className={
                                    'flex items-center justify-center focus:outline-none text-white text-sm ' +
                                    'sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition ' +
                                    'duration-150 ease-in'
                                }
                            >
                                <span className="mr-2 uppercase">Set username</span>
                                <span>
                                    <svg.SvgMask src={ArrowRightIcon} />
                                </span>
                            </button>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    )
}

export default SetUsername
