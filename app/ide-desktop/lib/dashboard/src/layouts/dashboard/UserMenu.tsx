/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as appUtils from '#/appUtils'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import ChangePasswordModal from '#/layouts/dashboard/ChangePasswordModal'
import * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as github from '#/utilities/github'
import * as shortcuts from '#/utilities/shortcuts'

import MenuEntry from '#/components/MenuEntry'
import Modal from '#/components/Modal'

// ================
// === UserMenu ===
// ================

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
  setPage: (page: pageSwitcher.Page) => void
  supportsLocalBackend: boolean
  onSignOut: () => void
}

/** Handling the UserMenuItem click event logic and displaying its content. */
export default function UserMenu(props: UserMenuProps) {
  const { setPage, supportsLocalBackend, onSignOut } = props
  const navigate = navigateHooks.useNavigate()
  const { signOut } = authProvider.useAuth()
  const { accessToken, organization } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  // The shape of the JWT payload is statically known.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const username: string | null =
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
    accessToken != null ? JSON.parse(atob(accessToken.split('.')[1]!)).username : null
  const canChangePassword = username != null ? !/^Github_|^Google_/.test(username) : false

  return (
    <Modal className="absolute overflow-hidden bg-dim w-full h-full">
      <div
        data-testid="user-menu"
        className="absolute flex flex-col bg-frame-selected backdrop-blur-3xl rounded-2xl gap-3 right-2.25 top-2.25 w-51.5 px-2 py-2.25"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        {organization != null ? (
          <>
            <div className="flex items-center gap-3 px-1">
              <div className="flex items-center rounded-full overflow-clip w-7.25 h-7.25">
                <img src={organization.profilePicture ?? DefaultUserIcon} height={28} width={28} />
              </div>
              <span className="leading-170 h-6 py-px">{organization.name}</span>
            </div>
            <div className="flex flex-col">
              {canChangePassword && (
                <MenuEntry
                  action={shortcuts.KeyboardAction.changeYourPassword}
                  paddingClassName="p-1"
                  doAction={() => {
                    setModal(<ChangePasswordModal />)
                  }}
                />
              )}
              {!supportsLocalBackend && (
                <MenuEntry
                  action={shortcuts.KeyboardAction.downloadApp}
                  paddingClassName="p-1"
                  doAction={async () => {
                    unsetModal()
                    const downloadUrl = await github.getDownloadUrl()
                    if (downloadUrl == null) {
                      toastAndLog('Could not find a download link for the current OS')
                    } else {
                      window.open(downloadUrl, '_blank')
                    }
                  }}
                />
              )}
              <MenuEntry
                action={shortcuts.KeyboardAction.settings}
                paddingClassName="p-1"
                doAction={() => {
                  unsetModal()
                  setPage(pageSwitcher.Page.settings)
                }}
              />
              <MenuEntry
                action={shortcuts.KeyboardAction.signOut}
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
              <span className="leading-170 h-6 py-px">You are not logged in.</span>
            </div>
            <div className="flex flex-col">
              <MenuEntry
                action={shortcuts.KeyboardAction.signIn}
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
