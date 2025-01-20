/** @file A styled input for settings pages. */
import {
  Form,
  Input,
  Password,
  TEXT_STYLE,
  type FieldPath,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { tv } from '#/utilities/tailwindVariants'

const SETTINGS_FIELD_STYLES = tv({
  extend: Form.FIELD_STYLES,
  slots: {
    base: 'flex-row flex-wrap',
    labelContainer: 'flex min-h-row items-center gap-1.5 w-full',
    label: TEXT_STYLE({
      className: 'flex justify-center self-start w-40 h-10 shrink-0',
      variant: 'body',
    }),
    error: 'ml-[180px]',
  },
})

// =========================
// === SettingsAriaInput ===
// =========================

/** Props for a {@link SettingsAriaInput}. */
export type SettingsAriaInputProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
> = Omit<InputProps<Schema, TFieldName>, 'fieldVariants' | 'size' | 'variant' | 'variants'>

/** A styled input for settings pages. */
export function SettingsAriaInput<Schema extends TSchema, TFieldName extends FieldPath<Schema>>(
  props: SettingsAriaInputProps<Schema, TFieldName>,
) {
  return <Input fieldVariants={SETTINGS_FIELD_STYLES} {...props} />
}

/** A styled password input for settings pages. */
export function SettingsAriaInputPassword<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: SettingsAriaInputProps<Schema, TFieldName>) {
  return <Password fieldVariants={SETTINGS_FIELD_STYLES} {...props} />
}

/** A styled email input for settings pages. */
export function SettingsAriaInputEmail<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: SettingsAriaInputProps<Schema, TFieldName>) {
  return <Input fieldVariants={SETTINGS_FIELD_STYLES} type="email" {...props} />
}
