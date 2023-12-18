/** @file A placeholder component replacing `Chat` when a user is not logged in. */
import * as React from 'react'
import * as reactDom from 'react-dom'

import CloseLargeIcon from 'enso-assets/close_large.svg'

import * as animations from '../../animations'
import * as app from '../../components/app'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'

import * as chat from './chat'
import * as pageSwitcher from './pageSwitcher'

/** Props for a {@link ChatPlaceholder}. */
export interface ChatPlaceholderProps {
    page: pageSwitcher.Page
    /** This should only be false when the panel is closing. */
    isOpen: boolean
    doClose: () => void
}

/** A placeholder component replacing `Chat` when a user is not logged in. */
export default function ChatPlaceholder(props: ChatPlaceholderProps) {
    const { page, isOpen, doClose } = props
    const logger = loggerProvider.useLogger()
    const navigate = hooks.useNavigate()
    const [right, setTargetRight] = animations.useInterpolateOverTime(
        animations.interpolationFunctionEaseInOut,
        chat.ANIMATION_DURATION_MS,
        -chat.WIDTH_PX
    )

    const container = document.getElementById(chat.HELP_CHAT_ID)

    React.useEffect(() => {
        // The types come from a third-party API and cannot be changed.
        // eslint-disable-next-line no-restricted-syntax
        let handle: number | undefined
        if (container != null) {
            if (isOpen) {
                container.style.display = ''
                setTargetRight(0)
            } else {
                setTargetRight(-chat.WIDTH_PX)
                handle = window.setTimeout(() => {
                    container.style.display = 'none'
                }, chat.ANIMATION_DURATION_MS)
            }
        }
        return () => {
            clearTimeout(handle)
        }
    }, [isOpen, container, setTargetRight])

    if (container == null) {
        logger.error('Chat container not found.')
        return null
    } else {
        return reactDom.createPortal(
            <div
                style={{ right }}
                className={`text-xs text-chat flex flex-col fixed top-0 right-0 backdrop-blur-3xl h-screen border-ide-bg-dark border-l-2 w-83.5 py-1 z-1 ${
                    page === pageSwitcher.Page.editor ? 'bg-ide-bg' : 'bg-frame-selected'
                }`}
            >
                <div className="flex text-sm font-semibold mx-4 mt-2">
                    <div className="grow" />
                    <button className="mx-1" onClick={doClose}>
                        <img src={CloseLargeIcon} />
                    </button>
                </div>
                <div className="grow grid place-items-center">
                    <div className="flex flex-col gap-3 text-base text-center">
                        <div>
                            Login or register to access live chat
                            <br />
                            with our support team.
                        </div>
                        <button
                            className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-170 h-8 py-px px-2 w-min"
                            onClick={() => {
                                navigate(app.LOGIN_PATH)
                            }}
                        >
                            Login
                        </button>
                        <button
                            className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-170 h-8 py-px px-2 w-min"
                            onClick={() => {
                                navigate(app.REGISTRATION_PATH)
                            }}
                        >
                            Register
                        </button>
                    </div>
                </div>
            </div>,
            container
        )
    }
}
