/** @file A placeholder component replacing `Chat` when a user is not logged in. */
import * as React from 'react'

import * as reactDom from 'react-dom'

import CloseLargeIcon from 'enso-assets/close_large.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'

import * as loggerProvider from '#/providers/LoggerProvider'
import * as textProvider from '#/providers/TextProvider'

import * as chat from '#/layouts/Chat'

import UnstyledButton from '#/components/UnstyledButton'

/** Props for a {@link ChatPlaceholder}. */
export interface ChatPlaceholderProps {
  /** This should only be false when the panel is closing. */
  readonly isOpen: boolean
  readonly doClose: () => void
}

/** A placeholder component replacing `Chat` when a user is not logged in. */
export default function ChatPlaceholder(props: ChatPlaceholderProps) {
  const { isOpen, doClose } = props
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()
  const navigate = navigateHooks.useNavigate()

  const container = document.getElementById(chat.HELP_CHAT_ID)

  if (container == null) {
    logger.error('Chat container not found.')
    return null
  } else {
    return reactDom.createPortal(
      <div
        className={`fixed right top z-1 flex h-screen w-chat flex-col py-chat-y text-xs text-primary shadow-soft backdrop-blur-default transition-transform ${isOpen ? '' : 'translate-x-full'}`}
      >
        <div className="mx-chat-header-x mt-chat-header-t flex text-sm font-semibold">
          <div className="grow" />
          <UnstyledButton className="mx-close-icon" onPress={doClose}>
            <img src={CloseLargeIcon} />
          </UnstyledButton>
        </div>
        <div className="grid grow place-items-center">
          <div className="flex flex-col gap-status-page text-center text-base">
            <div className="px-missing-functionality-text-x">
              {getText('placeholderChatPrompt')}
            </div>
            <UnstyledButton
              className="button self-center bg-help text-white"
              onPress={() => {
                navigate(appUtils.LOGIN_PATH)
              }}
            >
              {getText('login')}
            </UnstyledButton>
            <UnstyledButton
              className="button self-center bg-help text-white"
              onPress={() => {
                navigate(appUtils.REGISTRATION_PATH)
              }}
            >
              {getText('register')}
            </UnstyledButton>
          </div>
        </div>
      </div>,
      container
    )
  }
}
