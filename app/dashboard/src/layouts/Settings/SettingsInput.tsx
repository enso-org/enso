/** @file Rendering for an {@link SettingsInputData}. */
import SettingsAriaInput from '#/layouts/Settings/SettingsAriaInput'
import type { SettingsContext, SettingsInputData } from '#/layouts/Settings/settingsData'
import { useText } from '#/providers/TextProvider'

// =====================
// === SettingsInput ===
// =====================

/** Props for a {@link SettingsInput}. */
export interface SettingsInputProps<T extends Record<keyof T, string>> {
  readonly context: SettingsContext
  readonly data: SettingsInputData<T>
}

/** Rendering for an {@link SettingsInputData}. */
export default function SettingsInput<T extends Record<keyof T, string>>(
  props: SettingsInputProps<T>,
) {
  const { context, data } = props
  const { name, nameId, getValue, getEditable } = data
  const { getText } = useText()
  const value = getValue(context)
  const isEditable = getEditable(context)

  return (
    <SettingsAriaInput
      isDisabled={!isEditable}
      label={getText(nameId)}
      defaultValue={value}
      name={name}
    />
  )
}
