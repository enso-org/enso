/** @file Button bar for managing keyboard shortcuts. */
import * as React from 'react'

import type * as inputBindingsModule from '#/configurations/inputBindings'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

// =======================================
// === KeyboardShortcutsSettingsTabBar ===
// =======================================

/** Props for a {@link KeyboardShortcutsSettingsTabBar}. */
export interface KeyboardShortcutsSettingsTabBarProps {
  readonly doRefresh: () => void
}

/** Button bar for managing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsTabBar(
  props: KeyboardShortcutsSettingsTabBarProps
) {
  const { doRefresh } = props
  const inputBindings = inputBindingsManager.useInputBindings()
  const { setModal } = modalProvider.useSetModal()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLDivElement>(null)

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
          setModal(
            <ConfirmDeleteModal
              actionText="reset all keyboard shortcuts"
              actionButtonLabel="Reset All"
              doDelete={() => {
                for (const k in inputBindings.metadata) {
                  // eslint-disable-next-line no-restricted-syntax
                  inputBindings.reset(k as inputBindingsModule.DashboardBindingKey)
                }
                doRefresh()
              }}
            />
          )
        }}
      >
        <span className="text whitespace-nowrap font-semibold">Reset All</span>
      </button>
    </div>
  )
}
