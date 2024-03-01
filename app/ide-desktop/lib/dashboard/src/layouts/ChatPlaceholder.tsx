/** @file A placeholder component replacing `Chat` when a user is not logged in. */
import * as React from 'react'

import * as reactDom from 'react-dom'

import CloseLargeIcon from 'enso-assets/close_large.svg'
import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'

import * as loggerProvider from '#/providers/LoggerProvider'

import * as chat from '#/layouts/Chat'
import * as pageSwitcher from '#/layouts/PageSwitcher'

/** Props for a {@link ChatPlaceholder}. */
export interface ChatPlaceholderProps {
  readonly page: pageSwitcher.Page
  /** This should only be false when the panel is closing. */
  readonly isOpen: boolean
  readonly doClose: () => void
}

/** A placeholder component replacing `Chat` when a user is not logged in. */
export default function ChatPlaceholder(props: ChatPlaceholderProps) {
  const { page, isOpen, doClose } = props
  const logger = loggerProvider.useLogger()
  const navigate = navigateHooks.useNavigate()

  const container = document.getElementById(chat.HELP_CHAT_ID)

  if (container == null) {
    logger.error('Chat container not found.')
    return null
  } else {
    return reactDom.createPortal(
      <div
        className={`text-xs text-primary flex flex-col fixed top right backdrop-blur-default h-screen shadow-soft w-chat py-chat-y z-3 transition-transform ${
          detect.isGUI1() && page === pageSwitcher.Page.editor ? 'bg-ide-bg' : ''
        } ${isOpen ? '' : 'translate-x-full'}`}
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
              className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-cozy h-8 py-px px-2 w-min"
              onClick={() => {
                navigate(appUtils.LOGIN_PATH)
              }}
            >
              Login
            </button>
            <button
              className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-cozy h-8 py-px px-2 w-min"
              onClick={() => {
                navigate(appUtils.REGISTRATION_PATH)
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
