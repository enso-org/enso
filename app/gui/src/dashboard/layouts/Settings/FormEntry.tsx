/** @file Rendering for an {@link SettingsFormEntryData}. */
import { ButtonGroup, Form } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { AnimatePresence, motion } from 'framer-motion'
import { useEffect, useMemo, useRef, useState } from 'react'
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
  const { schema: schemaRaw, getValue, inputs, onSubmit, getVisible } = data

  const { getText } = useText()

  const visible = getVisible?.(context) ?? true
  const value = getValue(context)

  const [initialValueString] = useState(() => JSON.stringify(value))
  const valueStringRef = useRef(initialValueString)

  const schema = useMemo(
    () => (typeof schemaRaw === 'function' ? schemaRaw(context) : schemaRaw),
    [context, schemaRaw],
  )
  const isEditable = data.inputs.some((inputData) =>
    typeof inputData.editable === 'boolean' ?
      inputData.editable
    : (inputData.editable?.(context) ?? true),
  )

  const form = Form.useForm({
    // @ts-expect-error This is SAFE, as the type `T` is statically known.
    schema,
    defaultValues: value,
    onSubmit: async (newValue) => {
      // @ts-expect-error This is SAFE, as the type `T` is statically known.
      await onSubmit(context, newValue)
      form.reset(newValue)
      // The form should not be reset on error.
    },
  })

  useEffect(() => {
    const newValueString = JSON.stringify(value)

    if (newValueString !== valueStringRef.current) {
      form.reset(value)
      valueStringRef.current = newValueString
    }
  }, [form, value])

  if (!visible) return null

  const shouldShowSaveButton = isEditable && form.formState.isDirty

  return (
    <Form form={form}>
      {inputs.map((input) => (
        <SettingsInput key={input.name} context={context} data={input} />
      ))}

      <AnimatePresence>
        {shouldShowSaveButton && (
          <motion.div
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            exit={{ opacity: 0, y: -10 }}
          >
            <ButtonGroup>
              <Form.Submit>{getText('save')}</Form.Submit>
              <Form.Reset>{getText('cancel')}</Form.Reset>
            </ButtonGroup>
          </motion.div>
        )}
      </AnimatePresence>

      <Form.FormError />
    </Form>
  )
}
