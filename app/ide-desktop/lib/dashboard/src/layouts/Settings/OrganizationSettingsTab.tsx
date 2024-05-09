/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import OrganizationProfilePictureSettingsSection from '#/layouts/Settings/OrganizationProfilePictureSettingsSection'
import OrganizationSettingsSection from '#/layouts/Settings/OrganizationSettingsSection'

import type * as backendModule from '#/services/Backend'

// ===============================
// === OrganizationSettingsTab ===
// ===============================

/** Props for a {@link OrganizationSettingsTab}. */
export interface OrganizationSettingsTabProps {
  readonly organization: backendModule.OrganizationInfo
  readonly setOrganization: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsTab(props: OrganizationSettingsTabProps) {
  return (
    <div className="flex-0 flex h min-h-full flex-1 flex-col gap-settings-section overflow-auto lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <OrganizationSettingsSection {...props} />
      </div>
      <OrganizationProfilePictureSettingsSection {...props} />
    </div>
  )
}
