/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

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
  const shouldMakeSpaceForExtendedEditorMenu = page === pageSwitcher.Page.editor
  const rootRef = React.useRef<HTMLDivElement>(null)
  const navigator2D = navigator2DProvider.useNavigator2D()
  const childrenLengthRef = React.useRef(0)
  childrenLengthRef.current = 2 + (shouldShowInviteButton ? 1 : 0) + (shouldShowShareButton ? 1 : 0)

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      axis: keyboardNavigationHooks.Axis.horizontal,
      length: childrenLengthRef.current,
    })
  const keyboardSelectedChildId =
    keyboardSelectedIndex == null
      ? null
      : (
          [
            'chat',
            ...(shouldShowInviteButton ? (['invite'] as const) : ([] as const)),
            ...(shouldShowShareButton ? (['share'] as const) : ([] as const)),
            'user-menu',
          ] as const
        )[keyboardSelectedIndex] ?? null

  React.useEffect(() => {
    const root = rootRef.current
    if (invisible || root == null) {
      return
    } else {
      navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
        focusWhenPressed: {
          left: () => {
            setKeyboardSelectedIndex(childrenLengthRef.current - 1)
          },
          right: setKeyboardSelectedIndex.bind(null, 0),
        },
      })
      return () => {
        navigator2D.unregister(root)
      }
    }
  }, [invisible, navigator2D, setKeyboardSelectedIndex])

  return (
    <div
      ref={rootRef}
      className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame px-icons-x pr-profile-picture backdrop-blur-default ${
        shouldMakeSpaceForExtendedEditorMenu ? 'mr-extended-editor-menu' : ''
      }`}
    >
      <Button
        ref={element => {
          if (keyboardSelectedChildId === 'chat') {
            element?.focus()
          }
        }}
        focusRing={keyboardSelectedChildId === 'chat'}
        active={isHelpChatOpen}
        image={ChatIcon}
        onClick={() => {
          setIsHelpChatOpen(!isHelpChatOpen)
        }}
      />
      {shouldShowInviteButton && (
        <button
          ref={element => {
            if (keyboardSelectedChildId === 'invite') {
              element?.focus()
            }
          }}
          className={`text my-auto rounded-full bg-share px-button-x text-inversed ${keyboardSelectedChildId === 'invite' ? 'focus-ring' : ''}`}
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
          ref={element => {
            if (keyboardSelectedChildId === 'share') {
              element?.focus()
            }
          }}
          className={`text my-auto rounded-full bg-share px-button-x text-inversed ${keyboardSelectedChildId === 'invite' ? 'focus-ring' : ''}`}
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
        ref={element => {
          if (keyboardSelectedChildId === 'user-menu') {
            element?.focus()
          }
        }}
        className={`flex size-profile-picture select-none items-center overflow-clip rounded-full ${keyboardSelectedChildId === 'user-menu' ? 'focus-ring' : ''}`}
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
