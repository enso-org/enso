/** @file Wraps another component in a visual tooltip. */
import type { PropsWithChildren, ReactElement } from 'react'

import type { Placement } from '#/components/aria'
import { Tooltip, TooltipTrigger } from '#/components/AriaComponents/Tooltip'

/** Props for a {@link WithVisualTooltip}. */
export interface WithVisualTooltipProps extends Readonly<PropsWithChildren> {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: ReactElement | string | false | null | undefined
  readonly tooltipPlacement?: Placement | undefined
}

/** Wrap an element in a visual tooltip. */
export function WithVisualTooltip(props: WithVisualTooltipProps) {
  const { tooltip, tooltipPlacement, children } = props
  const shouldShowTooltip = tooltip !== false && tooltip != null

  const tooltipElement = shouldShowTooltip ? tooltip : null

  if (tooltipElement == null) {
    return children
  }

  return (
    <TooltipTrigger delay={0} closeDelay={0}>
      {children}

      <Tooltip {...(tooltipPlacement != null ? { placement: tooltipPlacement } : {})}>
        {tooltipElement}
      </Tooltip>
    </TooltipTrigger>
  )
}
