/** @file Button bar for managing keyboard shortcuts. */
import * as React from 'react'

import type * as inputBindingsModule from '#/configurations/inputBindings'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'

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
    <FocusArea direction="horizontal">
      {(ref, innerProps) => (
        <div ref={ref} className="flex gap-drive-bar" {...innerProps}>
          <UnstyledButton
            className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
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
            <aria.Text className="text whitespace-nowrap font-semibold">
              {getText('resetAll')}
            </aria.Text>
          </UnstyledButton>
        </div>
      )}
    </FocusArea>
  )
}
