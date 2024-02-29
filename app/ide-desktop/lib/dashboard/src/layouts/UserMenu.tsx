/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

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
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  return (
    <Modal hidden={hidden} className="absolute overflow-hidden bg-dim w-full h-full">
      <div
        // The name comes from a third-party API and cannot be changed.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        {...(!hidden ? { 'data-testid': 'user-menu' } : {})}
        className="absolute flex flex-col bg-frame-selected backdrop-blur-3xl rounded-2xl gap-3 right-2.25 top-2.25 w-51.5 px-2 py-2.25"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        {user != null ? (
          <>
            <div className="flex items-center gap-3 px-1">
              <div className="flex items-center rounded-full overflow-clip w-7.25 h-7.25">
                <img
                  src={user.profilePicture ?? DefaultUserIcon}
                  height={28}
                  width={28}
                  className="pointer-events-none"
                />
              </div>
              <span className="leading-170 h-6 py-px">{user.name}</span>
            </div>
            <div className="flex flex-col">
              {!supportsLocalBackend && (
                <MenuEntry
                  action="downloadApp"
                  paddingClassName="p-1"
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
              <MenuEntry
                action="settings"
                paddingClassName="p-1"
                doAction={() => {
                  unsetModal()
                  setPage(pageSwitcher.Page.settings)
                }}
              />
              <MenuEntry
                action="signOut"
                paddingClassName="p-1"
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
            <div className="flex items-center h-7">
              <span className="leading-170 h-6 py-px">{getText('youAreNotLoggedIn')}</span>
            </div>
            <div className="flex flex-col">
              <MenuEntry
                action="signIn"
                paddingClassName="p-1"
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
