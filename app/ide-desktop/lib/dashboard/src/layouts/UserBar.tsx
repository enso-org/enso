/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as pageSwitcher from '#/layouts/PageSwitcher'
import UserMenu from '#/layouts/UserMenu'

import * as ariaComponents from '#/components/AriaComponents'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

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
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly projectAsset: backendModule.ProjectAsset | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
  const { backend, invisible = false, page, setPage, setIsHelpChatOpen } = props
  const { projectAsset, setProjectAsset, doRemoveSelf, onSignOut } = props
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
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
        <div className="bg-primary/5 pt-0.5">
          <div
            className="flex h-[46px] shrink-0 cursor-default items-center gap-user-bar pl-icons-x pr-3"
            {...innerProps}
          >
            <ariaComponents.Button
              variant="icon"
              size="custom"
              className="mr-1"
              icon={ChatIcon}
              aria-label={getText('openHelpChat')}
              onPress={() => {
                setIsHelpChatOpen(true)
              }}
            />

            {shouldShowInviteButton && (
              <ariaComponents.DialogTrigger>
                <ariaComponents.Button size="medium" variant="tertiary">
                  {getText('invite')}
                </ariaComponents.Button>

                <InviteUsersModal />
              </ariaComponents.DialogTrigger>
            )}

            <ariaComponents.Button variant="primary" size="medium" href={appUtils.SUBSCRIBE_PATH}>
              {getText('upgrade')}
            </ariaComponents.Button>
            {shouldShowShareButton && (
              <ariaComponents.Button
                size="medium"
                variant="tertiary"
                aria-label={getText('shareButtonAltText')}
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
                {getText('share')}
              </ariaComponents.Button>
            )}
            <Button
              active
              mask={false}
              alt={getText('userMenuAltText')}
              image={user?.profilePicture ?? DefaultUserIcon}
              buttonClassName="rounded-full after:rounded-full"
              className="h-row-h w-row-h rounded-full"
              onPress={() => {
                setModal(<UserMenu setPage={setPage} onSignOut={onSignOut} />)
              }}
            />
            {/* Required for shortcuts to work. */}
            <div className="hidden">
              <UserMenu hidden setPage={setPage} onSignOut={onSignOut} />
            </div>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
