/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import * as refreshHooks from '#/hooks/refreshHooks'

import * as textProvider from '#/providers/TextProvider'

import KeyboardShortcutsSettingsTabBar from '#/layouts/Settings/KeyboardShortcutsSettingsTabBar'
import KeyboardShortcutsTable from '#/layouts/Settings/KeyboardShortcutsTable'

import SettingsSection from '#/components/styled/settings/SettingsSection'

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsTab() {
  const { getText } = textProvider.useText()
  const [refresh, doRefresh] = refreshHooks.useRefresh()

  return (
    <SettingsSection noFocusArea title={getText('keyboardShortcuts')} className="w-full flex-1">
      <KeyboardShortcutsSettingsTabBar doRefresh={doRefresh} />
      <KeyboardShortcutsTable refresh={refresh} doRefresh={doRefresh} />
    </SettingsSection>
  )
}
