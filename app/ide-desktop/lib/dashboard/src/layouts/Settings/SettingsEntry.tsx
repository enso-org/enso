/** @file Rendering for an arbitrary {@link settingsData.SettingsEntryData}. */
import * as React from 'react'

import SettingsCustomEntry from '#/layouts/Settings/SettingsCustomEntry'
import * as settingsData from '#/layouts/Settings/settingsData'
import SettingsInputEntry from '#/layouts/Settings/SettingsInputEntry'

// =====================
// === SettingsEntry ===
// =====================

/** Props for a {@link SettingsEntry}. */
export interface SettingsEntryProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsEntryData
}

/** Rendering for an arbitrary {@link settingsData.SettingsEntryData}. */
export default function SettingsEntry(props: SettingsEntryProps) {
  const { context, data } = props
  switch (data.type) {
    case settingsData.SettingsEntryType.input: {
      return <SettingsInputEntry context={context} data={data} />
    }
    case settingsData.SettingsEntryType.custom: {
      return <SettingsCustomEntry context={context} data={data} />
    }
  }
}
