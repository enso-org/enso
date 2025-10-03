/** @file Rendering for an {@link SettingsFormEntryData}. */
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { useText } from '$/providers/react'
import { useEffect, useRef, useState } from 'react'
import SettingsInput from './Input'
import type { SettingsContext, SettingsFormEntryData } from './data'

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
  const { schema: schemaRaw, getValue, inputs, onSubmit, getVisible } = data

  const { getText } = useText()

  const visible = getVisible?.(context) ?? true
  const value = getValue(context)

  const [initialValueString] = useState(() => JSON.stringify(value))
  const valueStringRef = useRef(initialValueString)

  const isEditable = data.inputs.some((inputData) =>
    typeof inputData.editable === 'boolean' ?
      inputData.editable
    : (inputData.editable?.(context) ?? true),
  )

  const form = Form.useForm({
    // @ts-expect-error This is SAFE, as the type `T` is statically known.
    schema: typeof schemaRaw === 'function' ? schemaRaw(context) : schemaRaw,
    defaultValues: value,
    onSubmit: (newValue) => {
      // @ts-expect-error This is SAFE, as the type `T` is statically known.
      return onSubmit(context, newValue)
    },
  })

  const { isDirty } = Form.useFormState({ form })

  useEffect(() => {
    const newValueString = JSON.stringify(value)

    if (newValueString !== valueStringRef.current) {
      form.reset(value)
      valueStringRef.current = newValueString
    }
  }, [form, value])

  if (!visible) return null

  const shouldShowSaveButton = isEditable && isDirty

  return (
    <Form form={form}>
      {inputs.map((input) => (
        <SettingsInput key={input.name} context={context} data={input} />
      ))}
      {shouldShowSaveButton && (
        <Button.Group>
          <Form.Submit>{getText('save')}</Form.Submit>
          <Form.Reset>{getText('cancel')}</Form.Reset>
        </Button.Group>
      )}
      <Form.FormError />
    </Form>
  )
}
