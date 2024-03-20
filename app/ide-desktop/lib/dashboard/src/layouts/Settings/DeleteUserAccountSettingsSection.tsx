/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

// ========================================
// === DeleteUserAccountSettingsSection ===
// ========================================

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLDivElement | null>(null)

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, { length: 1 })

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

  return (
    // This UI element does not appear anywhere else.
    // eslint-disable-next-line no-restricted-syntax
    <div className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]">
      <h3 className="settings-subheading text-danger">Danger Zone</h3>
      <div className="flex gap-buttons">
        <button
          ref={element => {
            if (keyboardSelectedIndex === 0) {
              element?.focus()
            }
          }}
          className="button bg-danger px-delete-user-account-button-x text-inversed opacity-full hover:opacity-full"
          onClick={event => {
            event.stopPropagation()
            setModal(
              <ConfirmDeleteUserModal
                doDelete={async () => {
                  await backend.deleteUser()
                  await signOut()
                }}
              />
            )
          }}
        >
          <span className="text inline-block">Delete this user account</span>
        </button>
        <span className="text my-auto">
          Once deleted, it will be gone forever. Please be certain.
        </span>
      </div>
    </div>
  )
}
