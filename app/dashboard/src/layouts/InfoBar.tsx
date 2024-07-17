/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from '#/assets/chat.svg'
import LogoIcon from '#/assets/enso_logo.svg'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import InfoMenu from '#/layouts/InfoMenu'

import * as ariaComponents from '#/components/AriaComponents'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

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
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame backdrop-blur-default"
          {...innerProps}
        >
          {/* FIXME [sb]: https://github.com/enso-org/cloud-v2/issues/1227
           * Make help chat work even when signed out. */}
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
          <ariaComponents.Button
            size="custom"
            variant="custom"
            className="flex size-row-h select-none items-center overflow-clip rounded-full"
            onPress={() => {
              setModal(<InfoMenu />)
            }}
          >
            <SvgMask
              src={LogoIcon}
              alt={getText('openInfoMenu')}
              className="pointer-events-none size-7"
            />
          </ariaComponents.Button>
          {/* Required for shortcuts to work. */}
          <div className="hidden">
            <InfoMenu hidden />
          </div>
        </div>
      )}
    </FocusArea>
  )
}
