/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import OrganizationProfilePictureSettingsSection from '#/layouts/Settings/OrganizationProfilePictureSettingsSection'
import OrganizationSettingsSection from '#/layouts/Settings/OrganizationSettingsSection'

import type Backend from '#/services/Backend'

// ===============================
// === OrganizationSettingsTab ===
// ===============================

/** Props for a {@link OrganizationSettingsTab}. */
export interface OrganizationSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsTab(props: OrganizationSettingsTabProps) {
  const { backend } = props

  return (
    <div className="flex-0 flex h min-h-full flex-1 flex-col gap-settings-section overflow-auto lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <OrganizationSettingsSection backend={backend} />
      </div>
      <OrganizationProfilePictureSettingsSection backend={backend} />
    </div>
  )
}
