/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from '#/assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import { Text } from '#/components/AriaComponents'
import MenuEntry from '#/components/MenuEntry'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'

import AboutModal from '#/modals/AboutModal'

import { Plan } from '#/services/Backend'

import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import * as tailwindMerge from '#/utilities/tailwindMerge'

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

/** Handling the UserMenuItem click event logic and displaying its content. */
export default function UserMenu(props: UserMenuProps) {
  const { hidden = false, goToSettingsPage, onSignOut } = props

  const [initialized, setInitialized] = React.useState(false)
  const localBackend = backendProvider.useLocalBackend()
  const { signOut } = authProvider.useAuth()
  const { user } = authProvider.useFullUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  React.useLayoutEffect(() => {
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
    <Modal hidden={hidden} className="absolute size-full overflow-hidden bg-dim">
      <div
        {...(!hidden ? { 'data-testid': 'user-menu' } : {})}
        className={tailwindMerge.twMerge(
          'absolute right-2 top-2 flex flex-col gap-user-menu rounded-default bg-selected-frame backdrop-blur-default transition-all duration-user-menu',
          initialized ? 'w-user-menu p-user-menu' : 'size-row-h'
        )}
        onClick={event => {
          event.stopPropagation()
        }}
      >
        <div
          className={tailwindMerge.twMerge(
            'flex items-center gap-icons overflow-hidden transition-all duration-user-menu',
            initialized && 'px-menu-entry'
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
          className={tailwindMerge.twMerge(
            'grid transition-all duration-user-menu',
            initialized ? 'grid-rows-1fr' : 'grid-rows-0fr'
          )}
        >
          <FocusArea direction="vertical">
            {innerProps => (
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
                      const downloadUrl = await github.getDownloadUrl()
                      if (downloadUrl == null) {
                        toastAndLog('noAppDownloadError')
                      } else {
                        download.download(downloadUrl)
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
    </Modal>
  )
}
