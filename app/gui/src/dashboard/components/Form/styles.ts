/**
 * @file
 *
 * Styles for form components.
 */
import * as twv from '#/utilities/tailwindVariants'

/** Props for form components. */
export type FormStyleProps = twv.VariantProps<typeof FORM_STYLES>
export const FORM_STYLES = twv.tv({
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
