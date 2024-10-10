/** @file A styled input for settings pages. */
import {
  Form,
  INPUT_STYLES,
  Input,
  TEXT_STYLE,
  type FieldPath,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { tv } from '#/utilities/tailwindVariants'

const SETTINGS_INPUT_STYLES = tv({
  extend: INPUT_STYLES,
  variants: {
    readOnly: {
      true: {
        base: 'opacity-100 focus-within:outline-0 border-transparent focus-within:border-transparent',
      },
    },
  },
  slots: {
    base: 'p-0 transition-[border-color,outline] outline-offset-2 focus-within:border-primary/50 focus-within:outline focus-within:outline-2 focus-within:outline-offset-0 focus-within:outline-primary border-0.5 border-primary/20 rounded-2xl',
    inputContainer: TEXT_STYLE({ disableLineHeightCompensation: true }),
    addonStart: 'px-1',
    textArea: 'h-6 rounded-full px-1',
    addonEnd: 'px-1',
    description: 'px-1',
  },
})

const SETTINGS_FIELD_STYLES = tv({
  extend: Form.FIELD_STYLES,
  slots: {
    base: 'flex-row flex-wrap',
    labelContainer: 'flex min-h-row items-center gap-5 w-full',
    label: TEXT_STYLE({
      className: 'text self-start w-40 shrink-0',
      variant: 'body',
    }),
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
      variant="custom"
      size="custom"
      variants={SETTINGS_INPUT_STYLES}
      fieldVariants={SETTINGS_FIELD_STYLES}
      {...props}
    />
  )
}
