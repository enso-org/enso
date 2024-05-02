/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import InfoMenu from '#/layouts/InfoMenu'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/UnstyledButton'

// ===============
// === InfoBar ===
// ===============

/** Props for a {@link InfoBar}. */
export interface InfoBarProps {
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
}

/** A toolbar containing chat and the user menu. */
export default function InfoBar(props: InfoBarProps) {
  const { isHelpChatOpen, setIsHelpChatOpen } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { updateModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame px-icons-x pr-profile-picture backdrop-blur-default"
          {...innerProps}
        >
          <Button
            active={isHelpChatOpen}
            image={ChatIcon}
            onPress={() => {
              setIsHelpChatOpen(!isHelpChatOpen)
            }}
          />
          <UnstyledButton
            className="flex size-profile-picture select-none items-center overflow-clip rounded-full"
            onPress={() => {
              updateModal(oldModal => (oldModal?.type === InfoMenu ? null : <InfoMenu />))
            }}
          >
            <img
              src={user?.profilePicture ?? DefaultUserIcon}
              alt={getText('openUserMenu')}
              className="pointer-events-none"
              height={28}
              width={28}
            />
          </UnstyledButton>
          {/* Required for shortcuts to work. */}
          <div className="hidden">
            <InfoMenu hidden />
          </div>
        </div>
      )}
    </FocusArea>
  )
}
