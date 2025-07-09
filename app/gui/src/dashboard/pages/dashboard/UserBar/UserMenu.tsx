/** @file A dropdown menu of user actions and settings. */
import { useToggleEnsoDevtools } from '#/components/Devtools'
import { Popover } from '#/components/Dialog'
import MenuEntry from '#/components/MenuEntry'
import { ProfilePicture } from '#/components/ProfilePicture'
import { Text } from '#/components/Text'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { AboutModal } from '#/modals/AboutModal'
import { unsetModal } from '#/providers/ModalProvider'
import { Plan } from '#/services/Backend'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import type { ModalApi } from '#/utilities/modal'
import { SUBSCRIBE_PATH } from '$/appUtils'
import { useBackends, useFullUserSession, useRouter, useSession, useText } from '$/providers/react'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import { useRef } from 'react'

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
}

/** A dropdown menu of user actions and settings. */
export function UserMenu(props: UserMenuProps) {
  const { goToSettingsPage, onSignOut } = props

  const { router } = useRouter()
  const { localBackend } = useBackends()
  const { signOut } = useSession()
  const { user } = useFullUserSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const toggleEnsoDevtools = useToggleEnsoDevtools()
  const aboutModalRef = useRef<ModalApi>(null)

  return (
    <>
      <Popover data-testid="user-menu" size="xxsmall">
        <div className="mb-2 flex select-none items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
          <ProfilePicture picture={user.profilePicture} name={user.name} />
          <div className="flex min-w-0 flex-col">
            <Text disableLineHeightCompensation variant="body" truncate="1" weight="semibold">
              {user.name}
            </Text>
            <Text disableLineHeightCompensation>{getText(user.plan)}</Text>
          </div>
        </div>
        <div className="flex flex-col overflow-hidden">
          {localBackend == null && (
            <MenuEntry
              action="downloadApp"
              doAction={async () => {
                unsetModal()
                const downloadUrl = await getDownloadUrl()
                if (downloadUrl == null) {
                  toastAndLog('noAppDownloadError')
                } else {
                  void download({ url: downloadUrl })
                }
              }}
            />
          )}
          <MenuEntry action="settings" doAction={goToSettingsPage} />
          <MenuEntry
            action="aboutThisApp"
            doAction={() => {
              aboutModalRef.current?.open()
            }}
          />
          {user.isEnsoTeamMember && IS_DEV_MODE && (
            <MenuEntry
              action="ensoDevtools"
              doAction={() => {
                toggleEnsoDevtools()
              }}
            />
          )}
          {(user.plan === Plan.free || user.plan === Plan.solo) && (
            <MenuEntry
              action="upgradePlan"
              doAction={() => {
                onSignOut()
                void router.push(SUBSCRIBE_PATH)
              }}
            />
          )}
          <MenuEntry
            action="signOut"
            doAction={() => {
              onSignOut()
              void signOut()
            }}
          />
        </div>
      </Popover>
      <AboutModal ref={aboutModalRef} />
    </>
  )
}
