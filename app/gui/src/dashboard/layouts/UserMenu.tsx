/** @file A dropdown menu of user actions and settings. */
import { Popover, Text } from '#/components/AriaComponents'
import MenuEntry from '#/components/MenuEntry'
import FocusArea from '#/components/styled/FocusArea'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AboutModal from '#/modals/AboutModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import { useNavigate } from 'react-router-dom'
import { LOGIN_PATH } from '../appUtils'
import { useToggleEnsoDevtools } from '../components/Devtools'
import { ProfilePicture } from '../components/ProfilePicture'
import { useSessionAPI } from '../providers/SessionProvider'

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
  /** If `true`, disables `data-testid` because it will not be visible. */
  readonly hidden?: boolean
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
}

/** A dropdown menu of user actions and settings. */
export default function UserMenu(props: UserMenuProps) {
  const { hidden = false, goToSettingsPage, onSignOut } = props

  const navigate = useNavigate()
  const localBackend = useLocalBackend()
  const { signOut } = useSessionAPI()
  const { user } = useFullUserSession()
  const { setModal, unsetModal } = useSetModal()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const toggleEnsoDevtools = useToggleEnsoDevtools()

  const entries = (
    <>
      {localBackend == null && (
        <MenuEntry
          action="downloadApp"
          doAction={async () => {
            unsetModal()
            const downloadUrl = await getDownloadUrl()
            if (downloadUrl == null) {
              toastAndLog('noAppDownloadError')
            } else {
              download(downloadUrl)
            }
          }}
        />
      )}
      <MenuEntry action="settings" doAction={goToSettingsPage} />
      <MenuEntry
        action="aboutThisApp"
        doAction={() => {
          setModal(<AboutModal />)
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

      <MenuEntry
        action="signOut"
        doAction={() => {
          onSignOut()
          void signOut().then(() => {
            navigate(LOGIN_PATH)
          })
        }}
      />
    </>
  )

  return hidden ? entries : (
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
        <FocusArea direction="vertical">
          {(innerProps) => (
            <div className="flex flex-col overflow-hidden" {...innerProps}>
              {entries}
            </div>
          )}
        </FocusArea>
      </Popover>
    )
}
