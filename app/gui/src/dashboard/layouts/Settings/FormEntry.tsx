/** @file Rendering for an {@link SettingsFormEntryData}. */
import { ButtonGroup, Form } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { useMemo } from 'react'
import SettingsInput from './Input'
import type { SettingsContext, SettingsFormEntryData } from './data'

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
  const { schema: schemaRaw, getValue, inputs, onSubmit } = data
  const { getText } = useText()
  const value = getValue(context)
  const schema = useMemo(
    () => (typeof schemaRaw === 'function' ? schemaRaw(context) : schemaRaw),
    [context, schemaRaw],
  )

  return (
    <Form
      gap="none"
      // @ts-expect-error This is SAFE, as the type `T` is statically known.
      schema={schema}
      defaultValues={value}
      // @ts-expect-error This is SAFE, as the type `T` is statically known.
      onSubmit={(newValue) => onSubmit(context, newValue)}
    >
      {inputs
        .filter((input) => context.isMatch(getText(input.nameId)))
        .map((input) => (
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
