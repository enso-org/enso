/** @file An option in a selector. */
import { Radio, type RadioProps } from '#/components/aria'
import * as React from 'react'
import type { VariantProps } from 'tailwind-variants'
import { SELECTOR_OPTION_STYLES } from '../variants'

/** Props for a {@link SelectorOption}. */
export interface SelectorOptionProps
  extends RadioProps,
    VariantProps<typeof SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const SelectorOption = React.forwardRef(function SelectorOption(
  props: SelectorOptionProps,
  ref: React.ForwardedRef<HTMLLabelElement>,
) {
  const { label, ...radioProps } = props
  const { className } = props

  return (
    <Radio
      ref={ref}
      {...radioProps}
      className={(renderProps) =>
        SELECTOR_OPTION_STYLES({
          className: typeof className === 'function' ? className(renderProps) : className,
        })
      }
    >
      {label}
    </Radio>
  )
})
