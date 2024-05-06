/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import MembersSettingsTabBar from '#/layouts/Settings/MembersSettingsTabBar'
import MembersTable from '#/layouts/Settings/MembersTable'

import SettingsPage from '#/components/styled/settings/SettingsPage'
import SettingsSection from '#/components/styled/settings/SettingsSection'

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab() {
  const { getText } = textProvider.useText()

  return (
    <SettingsPage>
      <SettingsSection noFocusArea title={getText('members')} className='overflow-hidden'>
        <MembersSettingsTabBar />
        <MembersTable allowDelete />
      </SettingsSection>
    </SettingsPage>
  )
}
