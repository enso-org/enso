/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import * as refreshHooks from '#/hooks/refreshHooks'

import * as textProvider from '#/providers/TextProvider'

import KeyboardShortcutsSettingsTabBar from '#/layouts/Settings/KeyboardShortcutsSettingsTabBar'
import KeyboardShortcutsTable from '#/layouts/Settings/KeyboardShortcutsTable'

import * as aria from '#/components/aria'

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsTab() {
  const { getText } = textProvider.useText()
  const [refresh, doRefresh] = refreshHooks.useRefresh()

  return (
    <div className="flex w-full flex-1 flex-col gap-settings-section-header">
      <aria.Heading level={2} className="settings-subheading">
        {getText('keyboardShortcuts')}
      </aria.Heading>
      <KeyboardShortcutsSettingsTabBar doRefresh={doRefresh} />
      <KeyboardShortcutsTable refresh={refresh} doRefresh={doRefresh} />
    </div>
  )
}
