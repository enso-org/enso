/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import InviteUsersModal from '#/modals/InviteUsersModal'

// =============================
// === MembersSettingsTabBar ===
// =============================

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar() {
  const { setModal } = modalProvider.useSetModal()
  const rootRef = React.useRef<HTMLDivElement>(null)
  const navigator2D = navigator2DProvider.useNavigator2D()

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
    <div ref={rootRef} className="flex gap-drive-bar">
      <button
        ref={element => {
          if (keyboardSelectedIndex === 0) {
            element?.focus()
          }
        }}
        className={`flex h-row items-center rounded-full bg-frame px-new-project-button-x ${keyboardSelectedIndex === 0 ? 'focus-ring' : ''}`}
        onClick={event => {
          event.stopPropagation()
          setModal(<InviteUsersModal eventTarget={null} />)
        }}
      >
        <span className="text whitespace-nowrap font-semibold">Invite Members</span>
      </button>
    </div>
  )
}
