/** @file Rendering for an {@link settingsData.SettingsInputEntryData}. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import type * as settingsData from '#/layouts/Settings/settingsData'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/SettingsInput'

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
  const { nameId, getValue, setValue, validate, getEditable } = data
  const { getText } = textProvider.useText()
  const ref = React.useRef<HTMLInputElement | null>(null)
  const value = getValue(context)
  const isEditable = getEditable(context)

  const onChange =
    validate == null
      ? null
      : () => {
          if (ref.current) {
            const errorMessage = validate(ref.current.value, context)
            ref.current.setCustomValidity(errorMessage === true ? '' : errorMessage)
          }
        }

  const input = (
    <SettingsInput
      ref={ref}
      isDisabled={!isEditable}
      key={value}
      type="text"
      onSubmit={newValue => {
        if (ref.current?.validity.valid === true) {
          void setValue(context, newValue, () => {
            if (ref.current) {
              ref.current.value = value
            }
          })
        }
      }}
    />
  )

  return (
    <aria.TextField
      key={value}
      defaultValue={value}
      className="flex h-row gap-settings-entry"
      {...(onChange ? { onChange } : {})}
    >
      <aria.Label className="text my-auto w-organization-settings-label">
        {getText(nameId)}
      </aria.Label>
      {validate ? (
        <div className="flex grow flex-col">
          {input}
          <aria.FieldError className="text-red-700" />
        </div>
      ) : (
        input
      )}
    </aria.TextField>
  )
}
