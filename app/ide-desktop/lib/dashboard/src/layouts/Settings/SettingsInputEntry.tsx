/** @file Rendering for an {@link settingsData.SettingsInputEntryData}. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import type * as settingsData from '#/layouts/Settings/settingsData'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/settings/SettingsInput'

// ==========================
// === SettingsInputEntry ===
// ==========================

/** Props for a {@link SettingsInputEntry}. */
export interface SettingsInputEntryProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsInputEntryData
}

/** Rendering for an {@link settingsData.SettingsInputEntryData}. */
export default function SettingsInputEntry(props: SettingsInputEntryProps) {
  const { context, data } = props
  const { nameId, getValue, setValue, getEditable } = data
  const { getText } = textProvider.useText()
  const ref = React.useRef<HTMLInputElement | null>(null)
  const value = getValue(context)
  const isEditable = getEditable(context)

  return (
    <aria.TextField key={value} defaultValue={value} className="flex h-row gap-settings-entry">
      <aria.Label className="text my-auto w-organization-settings-label">
        {getText(nameId)}
      </aria.Label>
      <SettingsInput
        ref={ref}
        isDisabled={!isEditable}
        key={value}
        type="text"
        onSubmit={newValue => {
          void setValue(context, newValue, () => {
            if (ref.current) {
              ref.current.value = value
            }
          })
        }}
      />
    </aria.TextField>
  )
}
