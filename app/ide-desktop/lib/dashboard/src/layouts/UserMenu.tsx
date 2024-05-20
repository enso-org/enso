/** @file The UserMenu component provides a dropdown menu of user actions and settings. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'

import * as aria from '#/components/aria'
import MenuEntry from '#/components/MenuEntry'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'

import AboutModal from '#/modals/AboutModal'

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
  readonly onSignOut: () => void
}

/** Handling the UserMenuItem click event logic and displaying its content. */
export default function UserMenu(props: UserMenuProps) {
  const { hidden = false, setPage, onSignOut } = props
  const [initialized, setInitialized] = React.useState(false)
  const navigate = navigateHooks.useNavigate()
  const localBackend = backendProvider.useLocalBackend()
  const { signOut } = authProvider.useAuth()
  const { user } = authProvider.useNonPartialUserSession()
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
        className={`right-top-bar-margin top-top-bar-margin absolute flex flex-col gap-user-menu rounded-default bg-selected-frame backdrop-blur-default transition-all duration-user-menu ${initialized ? 'w-user-menu p-user-menu' : 'p-profile-picture size-row-h'}`}
        onClick={event => {
          event.stopPropagation()
        }}
      >
        {user != null ? (
          <>
            <div
              className={`flex items-center gap-icons overflow-hidden transition-all duration-user-menu ${initialized ? 'px-menu-entry' : ''}`}
            >
              <div className="size-profile-picture flex shrink-0 items-center overflow-clip rounded-full">
                <img
                  src={user.profilePicture ?? DefaultUserIcon}
                  className="size-profile-picture pointer-events-none"
                />
              </div>
              <aria.Text className="text">{user.name}</aria.Text>
            </div>
            <div
              className={`grid transition-all duration-user-menu ${initialized ? 'grid-rows-1fr' : 'grid-rows-0fr'}`}
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
                    <MenuEntry
                      action="settings"
                      doAction={() => {
                        unsetModal()
                        setPage(pageSwitcher.Page.settings)
                      }}
                    />
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
          </>
        ) : (
          <>
            <div className="h-profile-picture flex items-center">
              <aria.Text className="text">{getText('youAreNotLoggedIn')}</aria.Text>
            </div>
            <div className="flex flex-col">
              {aboutThisAppMenuEntry}
              <MenuEntry
                action="settings"
                doAction={() => {
                  unsetModal()
                  setPage(pageSwitcher.Page.settings)
                }}
              />
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
