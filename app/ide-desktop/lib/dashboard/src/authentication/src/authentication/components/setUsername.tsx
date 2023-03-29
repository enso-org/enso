/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */

import * as auth from '../providers/auth'
import * as common from './common'
import * as hooks from '../../hooks'
import * as icons from '../../components/svg'
import * as utils from '../../utils'

// ===================
// === SetUsername ===
// ===================

function SetUsername() {
    const { setUsername } = auth.useAuth()
    const { accessToken, email } = auth.usePartialUserSession()

    const [username, bindUsername] = hooks.useInput('')

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
                        onSubmit={utils.handleEvent(() =>
                            setUsername(accessToken, username, email)
                        )}
                    >
                        <div className="flex flex-col mb-6">
                            <div className="relative">
                                <common.SvgIcon data={icons.PATHS.at} />

                                <common.Input
                                    {...bindUsername}
                                    id="username"
                                    type="text"
                                    name="username"
                                    placeholder="Username"
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
                                    <icons.Svg data={icons.PATHS.rightArrow} />
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
