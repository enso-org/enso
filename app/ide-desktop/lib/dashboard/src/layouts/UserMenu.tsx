/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'

import MenuEntry from '#/components/MenuEntry'
import Modal from '#/components/Modal'

import * as download from '#/utilities/download'
import * as github from '#/utilities/github'

// ================
// === UserMenu ===
// ================

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
  /** If `true`, disables `data-testid` because it will not be visible. */
  readonly hidden?: boolean
  readonly setPage: (page: pageSwitcher.Page) => void
  readonly supportsLocalBackend: boolean
  readonly onSignOut: () => void
}

/** Handling the UserMenuItem click event logic and displaying its content. */
export default function UserMenu(props: UserMenuProps) {
  const { hidden = false, setPage, supportsLocalBackend, onSignOut } = props
  const navigate = navigateHooks.useNavigate()
  const { signOut } = authProvider.useAuth()
  const { user } = authProvider.useNonPartialUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  return (
    <Modal hidden={hidden} className="absolute size-full overflow-hidden bg-dim">
      <div
        // The name comes from a third-party API and cannot be changed.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        {...(!hidden ? { 'data-testid': 'user-menu' } : {})}
        className="absolute right-top-bar-margin top-top-bar-margin flex w-user-menu flex-col gap-user-menu rounded-default bg-selected-frame px-user-menu-x py-user-menu-y backdrop-blur-default"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        {user != null ? (
          <>
            <div className="flex items-center gap-icons px-menu-entry">
              <div className="flex size-profile-picture items-center overflow-clip rounded-full">
                <img
                  src={user.profilePicture ?? DefaultUserIcon}
                  className="pointer-events-none size-profile-picture"
                />
              </div>
              <span className="text">{user.name}</span>
            </div>
            <div className="flex flex-col">
              {!supportsLocalBackend && (
                <MenuEntry
                  action="downloadApp"
                  doAction={async () => {
                    unsetModal()
                    const downloadUrl = await github.getDownloadUrl()
                    if (downloadUrl == null) {
                      toastAndLog('Could not find a download link for the current OS')
                    } else {
                      download.download(downloadUrl)
                    }
                  }}
                />
              )}
              <MenuEntry
                action="settings"
                doAction={() => {
                  unsetModal()
                  setPage(pageSwitcher.Page.settings)
                }}
              />
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
          </>
        ) : (
          <>
            <div className="flex h-profile-picture items-center">
              <span className="text">You are not logged in.</span>
            </div>
            <div className="flex flex-col">
              <MenuEntry
                action="signIn"
                doAction={() => {
                  navigate(appUtils.LOGIN_PATH)
                }}
              />
            </div>
          </>
        )}
      </div>
    </Modal>
  )
}
