/** @file Rendering for an {@link SettingsInputData}. */
import { useText } from '#/providers/TextProvider'
import SettingsAriaInput from './AriaInput'
import type { SettingsContext, SettingsInputData } from './data'

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
  const { name, nameId, editable } = data
  const { getText } = useText()
  const isEditable = typeof editable === 'function' ? editable(context) : editable ?? true

  return <SettingsAriaInput readOnly={!isEditable} label={getText(nameId)} name={name} />
}
