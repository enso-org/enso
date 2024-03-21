/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'
import UserMenu from '#/layouts/UserMenu'

import * as aria from '#/components/aria'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import InviteUsersModal from '#/modals/InviteUsersModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'

// ===============
// === UserBar ===
// ===============

/** Props for a {@link UserBar}. */
export interface UserBarProps {
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly supportsLocalBackend: boolean
  readonly page: pageSwitcher.Page
  readonly setPage: (page: pageSwitcher.Page) => void
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly projectAsset: backendModule.ProjectAsset | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
  const { invisible = false, page, setPage, isHelpChatOpen, setIsHelpChatOpen } = props
  const { supportsLocalBackend, projectAsset, setProjectAsset, doRemoveSelf, onSignOut } = props
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { setModal, updateModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const self =
    user != null
      ? projectAsset?.permissions?.find(
          permissions => permissions.user.user_email === user.email
        ) ?? null
      : null
  const shouldShowShareButton =
    backend.type === backendModule.BackendType.remote &&
    page === pageSwitcher.Page.editor &&
    projectAsset != null &&
    setProjectAsset != null &&
    self != null
  const shouldShowInviteButton =
    sessionType === authProvider.UserSessionType.full && !shouldShowShareButton

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {(ref, innerProps) => (
        <div
          ref={ref}
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame px-icons-x pr-profile-picture backdrop-blur-default"
          {...innerProps}
        >
          <Button
            active={isHelpChatOpen}
            image={ChatIcon}
            onPress={() => {
              setIsHelpChatOpen(!isHelpChatOpen)
            }}
          />
          {shouldShowInviteButton && (
            <FocusRing>
              <aria.Button
                className="text my-auto rounded-full bg-share px-button-x text-inversed"
                onPress={() => {
                  setModal(<InviteUsersModal eventTarget={null} />)
                }}
              >
                Invite
              </aria.Button>
            </FocusRing>
          )}
          {shouldShowShareButton && (
            <FocusRing>
              <aria.Button
                className="text my-auto rounded-full bg-share px-button-x text-inversed"
                onPress={() => {
                  setModal(
                    <ManagePermissionsModal
                      item={projectAsset}
                      setItem={setProjectAsset}
                      self={self}
                      doRemoveSelf={doRemoveSelf}
                      eventTarget={null}
                    />
                  )
                }}
              >
                Share
              </aria.Button>
            </FocusRing>
          )}
          <FocusRing>
            <aria.Button
              className="flex size-profile-picture select-none items-center overflow-clip rounded-full"
              onPress={() => {
                updateModal(oldModal =>
                  oldModal?.type === UserMenu ? null : (
                    <UserMenu
                      setPage={setPage}
                      supportsLocalBackend={supportsLocalBackend}
                      onSignOut={onSignOut}
                    />
                  )
                )
              }}
            >
              <img
                src={user?.profilePicture ?? DefaultUserIcon}
                alt="Open user menu"
                className="pointer-events-none"
                height={28}
                width={28}
              />
            </aria.Button>
          </FocusRing>
          {/* Required for shortcuts to work. */}
          <div className="hidden">
            <UserMenu
              hidden
              setPage={setPage}
              supportsLocalBackend={supportsLocalBackend}
              onSignOut={onSignOut}
            />
          </div>
        </div>
      )}
    </FocusArea>
  )
}
