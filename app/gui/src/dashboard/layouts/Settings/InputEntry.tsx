/** @file Rendering for an {@link SettingsInputEntryData}. */
import { useRef, useState } from 'react'

import { Button, FieldError, Form, Label, TextField } from '#/components/aria'
import { useText } from '#/providers/TextProvider'
import { getMessageOrToString } from '#/utilities/error'
import type { SettingsContext, SettingsInputEntryData } from './data'
import SettingsInput from './Input'

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
  readonly context: SettingsContext
  readonly data: SettingsInputEntryData
}

/** Rendering for an {@link SettingsInputEntryData}. */
export default function SettingsInputEntry(props: SettingsInputEntryProps) {
  const { context, data } = props
  const { nameId, getValue, setValue, validate, getEditable } = data
  const { getText } = useText()
  const [errorMessage, setErrorMessage] = useState('')
  const isSubmitting = useRef(false)
  const value = getValue(context)
  const isEditable = getEditable(context)

  const input = (
    <SettingsInput
      isDisabled={!isEditable}
      key={value}
      type="text"
      onSubmit={(event) => {
        // Technically causes the form to submit twice when pressing `Enter` due to `Enter`
        // also triggering the submit button.This is worked around by using a ref
        // tracking whether the form is currently being submitted.
        event.currentTarget.form?.requestSubmit()
      }}
    />
  )

  return (
    <Form
      validationErrors={{ [FIELD_NAME]: errorMessage }}
      onSubmit={async (event) => {
        event.preventDefault()
        if (!isSubmitting.current) {
          isSubmitting.current = true
          const [[, newValue] = []] = new FormData(event.currentTarget)
          if (typeof newValue === 'string') {
            setErrorMessage('')
            try {
              await setValue(context, newValue)
            } catch (error) {
              setErrorMessage(getMessageOrToString(error))
            }
          }
          isSubmitting.current = false
        }
      }}
    >
      <TextField
        key={value}
        name={FIELD_NAME}
        defaultValue={value}
        className="flex h-row items-center gap-settings-entry"
        {...(validate ? { validate: (newValue) => validate(newValue, context) } : {})}
      >
        <Label className="text my-auto w-organization-settings-label">{getText(nameId)}</Label>
        {validate ?
          <div className="flex grow flex-col">
            {input}
            <FieldError className="text-red-700" />
          </div>
        : input}
        <Button type="submit" className="sr-only" />
      </TextField>
    </Form>
  )
}
