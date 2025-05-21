/** @file An option in a selector. */
import { ListBoxItem, type ListBoxItemProps } from '#/components/aria'
import { MULTI_SELECTOR_OPTION_STYLES } from '#/components/AriaComponents/Inputs/MultiSelector/variants'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { forwardRef, type ForwardedRef } from 'react'

/** Props for a {@link MultiSelectorOption}. */
export interface MultiSelectorOptionProps
  extends ListBoxItemProps,
    VariantProps<typeof MULTI_SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const MultiSelectorOption = forwardRef(function MultiSelectorOption(
  props: MultiSelectorOptionProps,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const { label, size, rounded, color, variant, ...radioProps } = props
  const { className } = props

  return (
    <ListBoxItem
      ref={ref}
      {...radioProps}
      className={(renderProps) =>
        MULTI_SELECTOR_OPTION_STYLES({
          className: typeof className === 'function' ? className(renderProps) : className,
          size,
          rounded,
          color,
          variant,
        })
      }
    >
      {label}
    </ListBoxItem>
  )
})
