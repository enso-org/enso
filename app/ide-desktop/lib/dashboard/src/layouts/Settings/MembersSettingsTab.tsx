/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import MembersSettingsTabBar from '#/layouts/Settings/MembersSettingsTabBar'
import MembersTable from '#/layouts/Settings/MembersTable'

import SettingsPage from '#/components/styled/settings/SettingsPage'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import type Backend from '#/services/Backend'

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Props for a {@link MembersSettingsTab}. */
export interface MembersSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab(props: MembersSettingsTabProps) {
  const { backend } = props
  const { getText } = textProvider.useText()

  return (
    <SettingsPage>
      <SettingsSection noFocusArea title={getText('members')} className="overflow-hidden">
        <MembersSettingsTabBar backend={backend} />
        <MembersTable allowDelete backend={backend} />
      </SettingsSection>
    </SettingsPage>
  )
}
