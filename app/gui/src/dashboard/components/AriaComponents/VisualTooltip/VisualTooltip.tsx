/** @file Wraps another component in a visual tooltip. */
import { useRef, type PropsWithChildren, type ReactElement } from 'react'

import type { Placement } from '../types'
import { useVisualTooltip } from './useVisualTooltip'

/** Props for a {@link VisualTooltip}. */
export interface VisualTooltipProps extends Readonly<PropsWithChildren> {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: ReactElement | string | false | null | undefined
  readonly tooltipPlacement?: Placement | undefined
  readonly className?: string | undefined
}

/** Wrap an element in a visual tooltip. */
export function VisualTooltip(props: VisualTooltipProps) {
  const { tooltip, tooltipPlacement, className, children } = props
  const shouldShowTooltip = tooltip !== false && tooltip != null
  const ref = useRef<HTMLDivElement>(null)

  const tooltipElement = shouldShowTooltip ? tooltip : null

  const { tooltip: visualTooltip, targetProps } = useVisualTooltip({
    targetRef: ref,
    children: tooltipElement,
    isDisabled: !shouldShowTooltip,
    ...(tooltipPlacement && { overlayPositionProps: { placement: tooltipPlacement } }),
  })

  if (tooltipElement == null) {
    return children
  }

  return (
    <div ref={ref} className={className} {...targetProps}>
      {children} {visualTooltip}
    </div>
  )
}
