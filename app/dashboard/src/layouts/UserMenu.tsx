/** @file A dropdown menu of user actions and settings. */
import DefaultUserIcon from '#/assets/default_user.svg'
import { Popover, Text } from '#/components/AriaComponents'
import MenuEntry from '#/components/MenuEntry'
import FocusArea from '#/components/styled/FocusArea'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AboutModal from '#/modals/AboutModal'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { Plan } from '#/services/Backend'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'

// ================
// === UserMenu ===
// ================

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

  const localBackend = useLocalBackend()
  const { signOut } = useAuth()
  const { user } = useFullUserSession()
  const { setModal, unsetModal } = useSetModal()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  const aboutThisAppMenuEntry = (
    <MenuEntry
      action="aboutThisApp"
      doAction={() => {
        setModal(<AboutModal />)
      }}
    />
  )

  return (
    <Popover {...(!hidden ? { testId: 'user-menu' } : {})} size="xxsmall">
      <div className="mb-2 flex items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
        <div className="flex size-row-h shrink-0 items-center overflow-clip rounded-full">
          <img
            src={user.profilePicture ?? DefaultUserIcon}
            className="pointer-events-none size-row-h"
          />
        </div>

        <div className="flex min-w-0 flex-col">
          <Text disableLineHeightCompensation variant="body" truncate="1" weight="semibold">
            {user.name}
          </Text>

          <Text disableLineHeightCompensation>{getText(`${user.plan ?? Plan.free}`)}</Text>
        </div>
      </div>
      <FocusArea direction="vertical">
        {(innerProps) => (
          <div className="flex flex-col overflow-hidden" {...innerProps}>
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
            {aboutThisAppMenuEntry}
            <MenuEntry
              action="signOut"
              doAction={() => {
                onSignOut()
                // Wait until React has switched back to drive view, before signing out.
                window.setTimeout(() => {
                  void signOut()
                }, 0)
              }}
            />
          </div>
        )}
      </FocusArea>
    </Popover>
  )
}
