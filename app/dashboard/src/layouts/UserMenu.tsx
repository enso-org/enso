/** @file A dropdown menu of user actions and settings. */
import { useLayoutEffect, useState } from 'react'

import DefaultUserIcon from '#/assets/default_user.svg'
import { Dialog, Text } from '#/components/AriaComponents'
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
import { twMerge } from '#/utilities/tailwindMerge'

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

  const [initialized, setInitialized] = useState(false)
  const localBackend = useLocalBackend()
  const { signOut } = useAuth()
  const { user } = useFullUserSession()
  const { setModal, unsetModal } = useSetModal()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  useLayoutEffect(() => {
    setInitialized(true)
  }, [])

  const aboutThisAppMenuEntry = (
    <MenuEntry
      action="aboutThisApp"
      doAction={() => {
        setModal(<AboutModal />)
      }}
    />
  )

  return (
    <Dialog className="absolute size-full overflow-hidden bg-dim">
      <div
        {...(!hidden ? { 'data-testid': 'user-menu' } : {})}
        className={twMerge(
          'absolute right-2 top-2 flex flex-col gap-user-menu rounded-default bg-selected-frame backdrop-blur-default transition-all duration-user-menu',
          initialized ? 'w-user-menu p-user-menu' : 'size-row-h',
        )}
        onClick={(event) => {
          event.stopPropagation()
        }}
      >
        <div
          className={twMerge(
            'flex items-center gap-icons overflow-hidden transition-all duration-user-menu',
            initialized && 'px-menu-entry',
          )}
        >
          <div className="flex size-row-h shrink-0 items-center overflow-clip rounded-full">
            <img
              src={user.profilePicture ?? DefaultUserIcon}
              className="pointer-events-none size-row-h"
            />
          </div>

          <div className="flex flex-col">
            <Text disableLineHeightCompensation variant="body" truncate="1" weight="semibold">
              {user.name}
            </Text>

            <Text disableLineHeightCompensation>{getText(`${user.plan ?? Plan.free}`)}</Text>
          </div>
        </div>
        <div
          className={twMerge(
            'grid transition-all duration-user-menu',
            initialized ? 'grid-rows-1fr' : 'grid-rows-0fr',
          )}
        >
          <FocusArea direction="vertical">
            {(innerProps) => (
              <div
                aria-label={getText('userMenuLabel')}
                className="flex flex-col overflow-hidden"
                {...innerProps}
              >
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
        </div>
      </div>
    </Dialog>
  )
}
