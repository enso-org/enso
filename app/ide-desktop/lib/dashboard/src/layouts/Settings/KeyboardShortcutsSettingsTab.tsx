/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import * as refreshHooks from '#/hooks/refreshHooks'

import KeyboardShortcutsSettingsTabBar from '#/layouts/Settings/KeyboardShortcutsSettingsTabBar'
import KeyboardShortcutsTable from '#/layouts/Settings/KeyboardShortcutsTable'

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsTab() {
  const [refresh, doRefresh] = refreshHooks.useRefresh()

  return (
    <div className="flex w-full flex-1 flex-col gap-settings-section-header">
      <h3 className="settings-subheading">Keyboard shortcuts</h3>
      <KeyboardShortcutsSettingsTabBar doRefresh={doRefresh} />
      <KeyboardShortcutsTable refresh={refresh} doRefresh={doRefresh} />
    </div>
  )
}
