/** @file A menu containing info about the app. */
import { Popover } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import MenuEntry from '#/components/MenuEntry'
import { Text } from '#/components/Text'
import AboutModal from '#/modals/AboutModal'
import { setModal } from '#/providers/ModalProvider'
import { LOGIN_PATH } from '$/appUtils'
import { useAuth, useRouter, useSession, useText } from '$/providers/react'
import { PRODUCT_NAME } from 'enso-common'

/** Props for an {@link InfoMenu}. */
export interface InfoMenuProps {
  readonly hidden?: boolean
}

/** A menu containing info about the app. */
export default function InfoMenu(props: InfoMenuProps) {
  const { hidden = false } = props

  const { router } = useRouter()
  const { signOut } = useSession()
  const { session } = useAuth()
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
