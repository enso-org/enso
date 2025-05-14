/** @file A menu containing info about the app. */
import { LOGIN_PATH } from '#/appUtils'
import { Popover, Text } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import MenuEntry from '#/components/MenuEntry'
import AboutModal from '#/modals/AboutModal'
import { useAuth } from '#/providers/AuthProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useText } from '#/providers/TextProvider'
import { useRouterInReact } from '$/providers/react'
import { PRODUCT_NAME } from 'enso-common'

/** Props for an {@link InfoMenu}. */
export interface InfoMenuProps {
  readonly hidden?: boolean
}

/** A menu containing info about the app. */
export default function InfoMenu(props: InfoMenuProps) {
  const { hidden = false } = props

  const { router } = useRouterInReact()
  const { signOut } = useSessionAPI()
  const { session } = useAuth()
  const { setModal } = useSetModal()
  const { getText } = useText()

  return (
    <Popover {...(!hidden ? { 'data-testid': 'info-menu' } : {})} size="xxsmall">
      <div className="mb-2 flex items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
        <Icon icon="enso_logo" className="pointer-events-none h-7 w-7 text-primary" />
        <Text>{PRODUCT_NAME}</Text>
      </div>
      <div aria-label={getText('infoMenuLabel')} className="flex flex-col overflow-hidden">
        <MenuEntry
          action="aboutThisApp"
          doAction={() => {
            setModal(<AboutModal />)
          }}
        />
        {session && (
          <MenuEntry
            action="signOut"
            doAction={() => signOut().then(() => router.push(LOGIN_PATH))}
          />
        )}
      </div>
    </Popover>
  )
}
