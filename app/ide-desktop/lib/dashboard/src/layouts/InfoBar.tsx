/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import LogoIcon from 'enso-assets/enso_logo.svg'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import InfoMenu from '#/layouts/InfoMenu'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
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
  const { updateModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame px-profile-picture backdrop-blur-default"
          {...innerProps}
        >
          {/* FIXME [sb]: https://github.com/enso-org/cloud-v2/issues/1227
           * Make help chat work even when signed out.
           * Note that the original class for the `div` above is `pr-profile-picture px-icons-x`. */}
          {/* eslint-disable-next-line @typescript-eslint/no-unnecessary-condition */}
          {false && (
            <Button
              active={isHelpChatOpen}
              image={ChatIcon}
              onPress={() => {
                setIsHelpChatOpen(!isHelpChatOpen)
              }}
            />
          )}
          <UnstyledButton
            className="flex size-profile-picture select-none items-center overflow-clip rounded-full"
            onPress={() => {
              updateModal(oldModal => (oldModal?.type === InfoMenu ? null : <InfoMenu />))
            }}
          >
            <SvgMask
              src={LogoIcon}
              alt={getText('openInfoMenu')}
              className="pointer-events-none size-7"
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
