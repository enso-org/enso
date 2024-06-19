/** @file Button bar for managing keyboard shortcuts. */
import * as React from 'react'

import type * as inputBindingsModule from '#/configurations/inputBindings'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

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
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.ButtonGroup>
      <ariaComponents.Button
        variant="bar"
        onPress={() => {
          setModal(
            <ConfirmDeleteModal
              actionText={getText('resetAllKeyboardShortcuts')}
              actionButtonLabel={getText('resetAll')}
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
        {getText('resetAll')}
      </ariaComponents.Button>
    </ariaComponents.ButtonGroup>
  )
}
