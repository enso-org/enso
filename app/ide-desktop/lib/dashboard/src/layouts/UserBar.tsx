/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'
import UserMenu from '#/layouts/UserMenu'

import * as aria from '#/components/aria'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// ===============
// === UserBar ===
// ===============

/** Props for a {@link UserBar}. */
export interface UserBarProps {
  readonly backend: Backend | null
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
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
  const { backend, invisible = false, page, setPage } = props
  const { isHelpChatOpen, setIsHelpChatOpen, projectAsset, setProjectAsset } = props
  const { doRemoveSelf, onSignOut } = props

  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { setModal, updateModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const self =
    user != null
      ? projectAsset?.permissions?.find(
          backendModule.isUserPermissionAnd(permissions => permissions.user.userId === user.userId)
        ) ?? null
      : null
  const shouldShowShareButton =
    backend != null &&
    page === pageSwitcher.Page.editor &&
    projectAsset != null &&
    setProjectAsset != null &&
    self != null
  const shouldShowInviteButton =
    backend != null && sessionType === authProvider.UserSessionType.full && !shouldShowShareButton

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {innerProps => (
        <div
          className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full px-icons-x"
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
            <UnstyledButton
              variant="accent"
              onPress={event => {
                const rect = event.target.getBoundingClientRect()
                const position = { pageX: rect.left, pageY: rect.top }
                setModal(<InviteUsersModal backend={backend} event={position} />)
              }}
            >
              <aria.Text slot="label">{getText('invite')}</aria.Text>
            </UnstyledButton>
          )}
          {shouldShowShareButton && (
            <UnstyledButton
              variant="accent"
              className="text my-auto h-row rounded-full bg-accent px-3 text-inversed"
              onPress={() => {
                setModal(
                  <ManagePermissionsModal
                    backend={backend}
                    item={projectAsset}
                    setItem={setProjectAsset}
                    self={self}
                    doRemoveSelf={doRemoveSelf}
                    eventTarget={null}
                  />
                )
              }}
            >
              <aria.Text slot="label">{getText('share')}</aria.Text>
            </UnstyledButton>
          )}
          <UnstyledButton
            className="size-row flex select-none items-center overflow-clip rounded-full transition-colors hover:bg-black/10"
            onPress={() => {
              updateModal(oldModal =>
                oldModal?.type === UserMenu ? null : (
                  <UserMenu setPage={setPage} onSignOut={onSignOut} />
                )
              )
            }}
          >
            <img
              src={user?.profilePicture ?? DefaultUserIcon}
              alt={getText('openUserMenu')}
              className="pointer-events-none"
              height={32}
              width={32}
            />
          </UnstyledButton>
          {/* Required for shortcuts to work. */}
          <div className="hidden">
            <UserMenu hidden setPage={setPage} onSignOut={onSignOut} />
          </div>
        </div>
      )}
    </FocusArea>
  )
}
