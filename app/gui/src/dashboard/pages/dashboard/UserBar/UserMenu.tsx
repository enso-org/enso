/** @file A dropdown menu of user actions and settings. */
import { useToggleEnsoDevtools } from '#/components/Devtools'
import { Popover } from '#/components/Dialog'
import MenuEntry from '#/components/MenuEntry'
import { ProfilePicture } from '#/components/ProfilePicture'
import { Text } from '#/components/Text'
import { useMenuEntries } from '#/hooks/menuHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { AboutModal } from '#/modals/AboutModal'
import { unsetModal } from '#/providers/ModalProvider'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import { SUBSCRIBE_PATH } from '$/appUtils'
import { useBackends, useFullUserSession, useRouter, useSession, useText } from '$/providers/react'
import { Plan } from 'enso-common/src/services/Backend'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'

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
  const session = useFullUserSession()
  const { user } = session
  const isCloudDataUnavailable = session.isCloudDataUnavailable ?? false
  const isNotSignedIn = session.isNotSignedIn ?? false
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const toggleEnsoDevtools = useToggleEnsoDevtools()

  const entries = useMenuEntries([
    localBackend == null && {
      action: 'downloadApp',
      doAction: () => {
        unsetModal()
        void getDownloadUrl().then((downloadUrl) => {
          if (downloadUrl == null) {
            toastAndLog('noAppDownloadError')
          } else {
            void download({ url: downloadUrl })
          }
        })
      },
    },
    { action: 'settings', doAction: goToSettingsPage },
    {
      action: 'aboutThisApp',
      doAction: () => {
        AboutModal.open()
      },
    },
    user.isEnsoTeamMember &&
      IS_DEV_MODE && {
        action: 'toggleEnsoDevtools',
        doAction: () => {
          toggleEnsoDevtools()
        },
      },
    !isCloudDataUnavailable &&
      (user.plan === Plan.free || user.plan === Plan.solo) && {
        action: 'upgradePlan',
        doAction: () => {
          onSignOut()
          void router.push(SUBSCRIBE_PATH)
        },
      },
    // With no identity there is nobody to sign out; the attempt would only fail against the
    // unreachable Cognito host.
    !isNotSignedIn && {
      action: 'signOut',
      doAction: () => {
        onSignOut()
        void signOut()
      },
    },
  ])

  return (
    <>
      <Popover data-testid="user-menu" size="xxsmall">
        <div className="mb-2 flex select-none items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
          <ProfilePicture picture={user.profilePicture} name={user.name} />
          <div className="flex min-w-0 flex-col">
            <Text disableLineHeightCompensation variant="body" truncate="1" weight="semibold">
              {isNotSignedIn ? getText('notSignedIn') : user.name}
            </Text>
            <Text disableLineHeightCompensation>
              {isCloudDataUnavailable ? getText('cloudDataUnavailableTitle') : getText(user.plan)}
            </Text>
          </div>
        </div>
        <div className="flex flex-col overflow-hidden">
          {entries.flatMap((entry) => {
            if (entry == null || entry === false) {
              return []
            }
            return [<MenuEntry key={entry.action} {...entry} />]
          })}
        </div>
      </Popover>
    </>
  )
}
