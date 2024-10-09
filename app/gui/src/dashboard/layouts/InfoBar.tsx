/** @file A toolbar containing chat and the user menu. */
import ChatIcon from '#/assets/chat.svg'
import LogoIcon from '#/assets/enso_logo.svg'
import { Button, DialogTrigger } from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
import InfoMenu from '#/layouts/InfoMenu'
import { useText } from '#/providers/TextProvider'

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
  const { getText } = useText()

  return (
    <FocusArea direction="horizontal">
      {(innerProps) => (
        <div
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame backdrop-blur-default"
          {...innerProps}
        >
          {/* FIXME [sb]: https://github.com/enso-org/cloud-v2/issues/1227
           * Make help chat work even when signed out. */}
          {/* eslint-disable-next-line @typescript-eslint/no-unnecessary-condition, no-constant-binary-expression */}
          {false && (
            <Button
              size="custom"
              variant="custom"
              isActive={isHelpChatOpen}
              icon={ChatIcon}
              onPress={() => {
                setIsHelpChatOpen(!isHelpChatOpen)
              }}
            />
          )}
          <DialogTrigger>
            <Button
              size="custom"
              variant="icon"
              className="flex size-row-h select-none items-center overflow-clip rounded-full"
            >
              <SvgMask
                src={LogoIcon}
                alt={getText('openInfoMenu')}
                className="pointer-events-none size-7"
              />
            </Button>
            <InfoMenu />
          </DialogTrigger>
          {/* Required for shortcuts to work. */}
          <div className="hidden">
            <InfoMenu hidden />
          </div>
        </div>
      )}
    </FocusArea>
  )
}
