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
  const columns = React.useMemo<readonly (readonly settingsData.SettingsSectionData[])[]>(() => {
    const result: settingsData.SettingsSectionData[][] = []
    for (const section of sections) {
      const columnNumber = section.column ?? 1
      let column = result[columnNumber - 1]
      if (column == null) {
        while (result.length + 1 < columnNumber) {
          result.push([])
        }
        column = []
        result.push(column)
      }
      column.push(section)
    }
    return result
  }, [sections])

  return columns.length === 1 ? (
    <div className="flex min-w-settings-main-section flex-col gap-settings-subsection">
      {sections.map(section => (
        <SettingsSection key={section.nameId} context={context} data={section} />
      ))}
    </div>
  ) : (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      {columns.map((sectionsInColumn, i) => (
        <div key={i} className="flex min-w-settings-main-section flex-col gap-settings-subsection">
          {sectionsInColumn.map(section => (
            <SettingsSection key={section.nameId} context={context} data={section} />
          ))}
        </div>
      ))}
    </div>
  )
}
