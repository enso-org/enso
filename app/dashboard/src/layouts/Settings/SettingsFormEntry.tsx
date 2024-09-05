/** @file Rendering for an {@link SettingsFormEntryData}. */
import { Button, ButtonGroup, Form } from '#/components/AriaComponents'
import SettingsInput from '#/layouts/Settings/SettingsInput'
import type { SettingsContext, SettingsFormEntryData } from '#/layouts/Settings/settingsData'
import { useText } from '#/providers/TextProvider'
import { useMemo } from 'react'

// =========================
// === SettingsFormEntry ===
// =========================

/** Props for a {@link SettingsFormEntry}. */
export interface SettingsFormEntryProps<T extends Record<keyof T, string>> {
  readonly context: SettingsContext
  readonly data: SettingsFormEntryData<T>
}

/** Rendering for an {@link SettingsFormEntryData}. */
export function SettingsFormEntry<T extends Record<keyof T, string>>(
  props: SettingsFormEntryProps<T>,
) {
  const { context, data } = props
  const { inputs, onSubmit, schema: schemaRaw } = data
  const { getText } = useText()
  const schema = useMemo(
    () => (typeof schemaRaw === 'function' ? schemaRaw(context) : schemaRaw),
    [],
  )

  return (
    // @ts-expect-error This is SAFE, as the type `T` is statically known.
    <Form schema={schema} gap="none" onSubmit={(value) => onSubmit(context, value)}>
      {inputs.map((input) => (
        <SettingsInput key={input.name} context={context} data={input} />
      ))}
      <ButtonGroup>
        <Form.Submit>{getText('save')}</Form.Submit>
        <Form.Reset>{getText('cancel')}</Form.Reset>
      </ButtonGroup>
      <Form.FormError />
    </Form>
  )
}
