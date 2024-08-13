/** @file A styled input for settings pages. */
import {
  Form,
  INPUT_STYLES,
  Input,
  type FieldPath,
  type FieldValues,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { tv } from '#/utilities/tailwindVariants'

const SETTINGS_INPUT_STYLES = tv({
  extend: INPUT_STYLES,
  slots: {
    base: 'p-0',
    textArea: 'rounded-2xl border-0.5 border-primary/20 px-1',
  },
})

const SETTINGS_FIELD_STYLES = tv({
  extend: Form.FIELD_STYLES,
  slots: {
    base: 'flex-row flex-wrap',
    labelContainer: 'flex min-h-row items-center gap-5 w-full',
    label: 'text mb-auto w-40 shrink-0',
    error: 'ml-[180px]',
  },
})

// =========================
// === SettingsAriaInput ===
// =========================

/** Props for a {@link SettingsAriaInput}. */
export interface SettingsAriaInputProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends Omit<
    InputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
    'fieldVariants' | 'size' | 'variant' | 'variants'
  > {}

/** A styled input for settings pages. */
export default function SettingsAriaInput<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(props: SettingsAriaInputProps<Schema, TFieldValues, TFieldName, TTransformedValues>) {
  return (
    <Input
      {...props}
      variant="custom"
      size="custom"
      variants={SETTINGS_INPUT_STYLES}
      fieldVariants={SETTINGS_FIELD_STYLES}
    />
  )
}
