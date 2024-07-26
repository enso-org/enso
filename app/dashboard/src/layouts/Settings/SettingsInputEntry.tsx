/** @file Rendering for an {@link settingsData.SettingsInputEntryData}. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import type * as settingsData from '#/layouts/Settings/settingsData'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/SettingsInput'

import * as errorModule from '#/utilities/error'

// =================
// === Constants ===
// =================

/** The name of the single field in this form. */
const FIELD_NAME = 'value'

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
  const [errorMessage, setErrorMessage] = React.useState('')
  const value = getValue(context)
  const isEditable = getEditable(context)

  const input = (
    <SettingsInput
      isDisabled={!isEditable}
      key={value}
      type="text"
      onSubmit={(event) => {
        event.currentTarget.form?.requestSubmit()
      }}
    />
  )

  return (
    <aria.Form
      validationErrors={{ [FIELD_NAME]: errorMessage }}
      onSubmit={async (event) => {
        event.preventDefault()
        const [[, newValue] = []] = new FormData(event.currentTarget)
        if (typeof newValue === 'string') {
          setErrorMessage('')
          try {
            await setValue(context, newValue)
          } catch (error) {
            setErrorMessage(errorModule.getMessageOrToString(error))
          }
        }
      }}
    >
      <aria.TextField
        key={value}
        name={FIELD_NAME}
        defaultValue={value}
        className="flex h-row gap-settings-entry"
        {...(validate ? { validate: (newValue) => validate(newValue, context) } : {})}
      >
        <aria.Label className="text my-auto w-organization-settings-label">
          {getText(nameId)}
        </aria.Label>
        {validate ?
          <div className="flex grow flex-col">
            {input}
            <aria.FieldError className="text-red-700" />
          </div>
        : input}
        <aria.Button type="submit" className="sr-only" />
      </aria.TextField>
    </aria.Form>
  )
}
