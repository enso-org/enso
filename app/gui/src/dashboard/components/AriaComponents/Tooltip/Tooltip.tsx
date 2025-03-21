/** @file Displays the description of an element on hover or focus. */
import * as aria from '#/components/aria'
import { useStrictPortalContext } from '#/components/Portal'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { ResetButtonGroupContext } from '../Button/ResetButtonGroupContext'
import { TOOLTIP_STYLES } from './variants'

const DEFAULT_CONTAINER_PADDING = 6
const DEFAULT_OFFSET = 9

/** Props for a {@link Tooltip}. */
export interface TooltipProps
  extends Omit<Readonly<aria.TooltipProps>, 'offset' | 'UNSTABLE_portalContainer'>,
    Omit<VariantProps<typeof TOOLTIP_STYLES>, 'isEntering' | 'isExiting'> {}

/** Displays the description of an element on hover or focus. */
export function Tooltip(props: TooltipProps) {
  const {
    className,
    containerPadding = DEFAULT_CONTAINER_PADDING,
    variant,
    size,
    rounded,
    variants = TOOLTIP_STYLES,
    ...ariaTooltipProps
  } = props

  const root = useStrictPortalContext()

  return (
    <ResetButtonGroupContext>
      <aria.Tooltip
        offset={DEFAULT_OFFSET}
        containerPadding={containerPadding}
        UNSTABLE_portalContainer={root}
        className={aria.composeRenderProps(className, (classNames, values) =>
          variants({ className: classNames, variant, size, rounded, ...values }),
        )}
        data-ignore-click-outside
        {...ariaTooltipProps}
      />
    </ResetButtonGroupContext>
  )
}

// Re-export the TooltipTrigger component from `react-aria-components`
// eslint-disable-next-line no-restricted-syntax
export const TooltipTrigger = aria.TooltipTrigger

Tooltip.Trigger = TooltipTrigger
