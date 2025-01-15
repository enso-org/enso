/**
 * @file
 * Component that renders an blurry underlay, that matches our design guidelines
 * Useful when you want to display a semi transparent element
 * above the other one.
 */
import { DIALOG_BACKGROUND } from '#/components/AriaComponents/Dialog'
import type { VariantProps } from '#/utilities/tailwindVariants'
import type { HTMLAttributes } from 'react'

/**
 * Props for the {@link Underlay} component.
 */
export interface UnderlayProps
  extends HTMLAttributes<HTMLDivElement>,
    VariantProps<typeof DIALOG_BACKGROUND> {}

/**
 * Component that renders an blurry underlay, that matches our design guidelines
 * Useful when you want to display a semi transparent element
 * above the other one.
 */
export function Underlay(props: UnderlayProps) {
  const { className, variants = DIALOG_BACKGROUND, variant, ...rest } = props

  return <div className={variants({ className, variant })} {...rest} />
}
