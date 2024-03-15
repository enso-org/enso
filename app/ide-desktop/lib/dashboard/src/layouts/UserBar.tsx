/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'
import UserMenu from '#/layouts/UserMenu'

import Button from '#/components/Button'

import InviteUsersModal from '#/modals/InviteUsersModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'

// ===============
// === UserBar ===
// ===============

/** Props for a {@link UserBar}. */
export interface UserBarProps {
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
  const { supportsLocalBackend, page, setPage, isHelpChatOpen, setIsHelpChatOpen } = props
  const { projectAsset, setProjectAsset, doRemoveSelf, onSignOut } = props
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
  const shouldMakeSpaceForExtendedEditorMenu = page === pageSwitcher.Page.editor

  return (
    <div
      className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame px-icons-x pr-profile-picture backdrop-blur-default ${
        shouldMakeSpaceForExtendedEditorMenu ? 'mr-extended-editor-menu' : ''
      }`}
    >
      <Button
        active={isHelpChatOpen}
        image={ChatIcon}
        onClick={() => {
          setIsHelpChatOpen(!isHelpChatOpen)
        }}
      />
      {shouldShowInviteButton && (
        <button
          className="text my-auto rounded-full bg-share px-button-x text-inversed"
          onClick={event => {
            event.stopPropagation()
            setModal(<InviteUsersModal eventTarget={null} />)
          }}
        >
          Invite
        </button>
      )}
      {shouldShowShareButton && (
        <button
          className="text my-auto rounded-full bg-share px-button-x text-inversed"
          onClick={event => {
            event.stopPropagation()
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
        </button>
      )}
      <button
        className="flex size-profile-picture select-none items-center overflow-clip rounded-full"
        onClick={event => {
          event.stopPropagation()
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
      </button>
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
  )
}
