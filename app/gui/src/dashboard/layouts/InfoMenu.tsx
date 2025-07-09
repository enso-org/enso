/** @file A menu containing info about the app. */
import { Popover } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import MenuEntry from '#/components/MenuEntry'
import { Text } from '#/components/Text'
import { AboutModal } from '#/modals/AboutModal'
import { type ModalApi } from '#/utilities/modal'
import { LOGIN_PATH } from '$/appUtils'
import { useAuth, useRouter, useSession, useText } from '$/providers/react'
import { PRODUCT_NAME } from 'enso-common'
import { useRef } from 'react'

/** A menu containing info about the app. */
export function InfoMenu() {
  const { router } = useRouter()
  const { signOut } = useSession()
  const { session } = useAuth()
  const { getText } = useText()
  const aboutModalRef = useRef<ModalApi>(null)

  return (
    <>
      <Popover data-testid="info-menu" size="xxsmall">
        <div className="mb-2 flex items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
          <Icon icon="enso_logo" className="pointer-events-none h-7 w-7 text-primary" />
          <Text>{PRODUCT_NAME}</Text>
        </div>
        <div aria-label={getText('infoMenuLabel')} className="flex flex-col overflow-hidden">
          <MenuEntry
            action="aboutThisApp"
            doAction={() => {
              aboutModalRef.current?.open()
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
      <AboutModal ref={aboutModalRef} />
    </>
  )
}
