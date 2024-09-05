/** @file A styled input for settings pages. */
import {
  Form,
  INPUT_STYLES,
  Input,
  type FieldPath,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { tv } from '#/utilities/tailwindVariants'

const SETTINGS_INPUT_STYLES = tv({
  extend: INPUT_STYLES,
  variants: {
    disabled: {
      true: { base: 'opacity-100', textArea: 'border-transparent' },
    },
  },
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
  TFieldName extends FieldPath<Schema>,
> extends Omit<InputProps<Schema, TFieldName>, 'fieldVariants' | 'size' | 'variant' | 'variants'> {}

/** A styled input for settings pages. */
export default function SettingsAriaInput<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: SettingsAriaInputProps<Schema, TFieldName>) {
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
