/** @file Rendering for a settings section. */
import * as React from 'react'

import type * as settingsData from '#/layouts/Settings/settingsData'
import SettingsSection from '#/layouts/Settings/SettingsSection'

// ===================
// === SettingsTab ===
// ===================

/** Props for a {@link SettingsTab}. */
export interface SettingsTabProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsTabData
}

/** Styled content of a settings tab. */
export default function SettingsTab(props: SettingsTabProps) {
  const { context, data } = props
  const { sections } = data

  return (
    <div className="flex flex-col gap-settings-subsection">
      {sections.map(section => (
        <SettingsSection context={context} data={section} />
      ))}
    </div>
  )
}
