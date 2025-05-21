/** @file Styles for form components. */
import { TEXT_STYLE } from '#/components/AriaComponents/Text'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

/** Props for form components. */
export type FormStyleProps = VariantProps<typeof FORM_STYLES>
export const FORM_STYLES = tv({
  base: 'flex flex-col items-start',
  variants: {
    gap: {
      custom: '',
      none: 'gap-0',
      small: 'gap-2',
      medium: 'gap-4',
      large: 'gap-6',
    },
  },
  defaultVariants: {
    gap: 'medium',
  },
})

export const FIELD_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: {
    fullWidth: { true: 'w-full' },
    isInvalid: { true: { label: 'text-danger' } },
    isHidden: { true: { base: 'hidden' } },
  },
  slots: {
    fieldContent: 'contents',
    contextualHelp: '',
    labelContainer: 'flex gap-1 items-center',
    label: TEXT_STYLE({ variant: 'body' }),
    content: 'flex flex-col items-start w-full',
    description: TEXT_STYLE({ variant: 'body', color: 'disabled' }),
  },
  defaultVariants: { fullWidth: true },
})

export const FIELD_ERROR_STYLES = tv({
  base: TEXT_STYLE({ variant: 'body', color: 'danger', className: 'block' }),
  variants: { fullWidth: { true: 'w-full' } },
  defaultVariants: { fullWidth: true },
})
