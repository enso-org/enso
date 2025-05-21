/** @file An option in a selector. */
import { AnimatedBackground } from '#/components/AnimatedBackground'
import { Radio, type RadioProps } from '#/components/aria'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { forwardRef, memo, type ForwardedRef } from 'react'
import { SELECTOR_OPTION_STYLES } from './variants'

/** Props for a {@link SelectorOption}. */
export interface SelectorOptionProps
  extends RadioProps,
    VariantProps<typeof SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const SelectorOption = memo(
  forwardRef(function SelectorOption(
    props: SelectorOptionProps,
    ref: ForwardedRef<HTMLLabelElement>,
  ) {
    const {
      label,
      value,
      size,
      rounded,
      variant,
      className,
      variants = SELECTOR_OPTION_STYLES,
      ...radioProps
    } = props

    const styles = variants({ size, rounded, variant })

    return (
      <AnimatedBackground.Item
        value={value}
        className={styles.base()}
        animationClassName={styles.animation()}
      >
        <Radio
          ref={ref}
          {...radioProps}
          value={value}
          className={(renderProps) => {
            return styles.radio({
              className: typeof className === 'function' ? className(renderProps) : className,
              ...renderProps,
            })
          }}
        >
          {({ isHovered, isSelected, isPressed }) => (
            <>
              <div className={styles.hover({ isHovered, isSelected, isPressed })} />
              <span className="isolate">{label}</span>
            </>
          )}
        </Radio>
      </AnimatedBackground.Item>
    )
  }),
)
