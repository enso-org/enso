/** @file A menu containing info about the app. */
import { PRODUCT_NAME } from 'enso-common'

import LogoIcon from '#/assets/enso_logo.svg'
import { Popover, Text } from '#/components/AriaComponents'
import MenuEntry from '#/components/MenuEntry'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
import AboutModal from '#/modals/AboutModal'
import { useAuth } from '#/providers/AuthProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'

// ================
// === InfoMenu ===
// ================

/** Props for an {@link InfoMenu}. */
export interface InfoMenuProps {
  readonly hidden?: boolean
}

/** A menu containing info about the app. */
export default function InfoMenu(props: InfoMenuProps) {
  const { hidden = false } = props
  const { signOut, session } = useAuth()
  const { setModal } = useSetModal()
  const { getText } = useText()

  return (
    <Popover {...(!hidden ? { testId: 'info-menu' } : {})} size="xxsmall">
      <div className="mb-2 flex items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
        <SvgMask src={LogoIcon} className="pointer-events-none h-7 w-7 text-primary" />
        <Text>{PRODUCT_NAME}</Text>
      </div>
      <FocusArea direction="vertical">
        {(innerProps) => (
          <div
            aria-label={getText('infoMenuLabel')}
            className="flex flex-col overflow-hidden"
            {...innerProps}
          >
            <MenuEntry
              action="aboutThisApp"
              doAction={() => {
                setModal(<AboutModal />)
              }}
            />
            {session && <MenuEntry action="signOut" doAction={signOut} />}
          </div>
        )}
      </FocusArea>
    </Popover>
  )
}
