/** @file A spotlight element. */
import Portal from '#/components/Portal'
import { useMeasure, type RectReadOnly } from '#/hooks/measureHooks'
import { convertCSSUnitString } from '#/utilities/convertCSSUnits'
import { startTransition, useEffect, useLayoutEffect, useState } from 'react'

/** Props for a {@link Spotlight}. */
export interface SpotlightProps {
  readonly element: HTMLElement | SVGElement | null
  readonly close: () => void
  readonly backgroundElement: HTMLElement | SVGElement
  readonly paddingPx?: number | undefined
}

/** A spotlight element. */
export function Spotlight(props: SpotlightProps) {
  const { element, close, paddingPx = 0 } = props

  const [bounds, setBounds] = useState<RectReadOnly>()
  const [borderRadius, setBorderRadius] = useState(0)

  const [dimensionsRef] = useMeasure({
    onResize: (nextBounds) => {
      startTransition(() => {
        setBounds(nextBounds)
      })
    },
  })

  useEffect(() => {
    if (element) {
      dimensionsRef(element)
    }
  }, [dimensionsRef, element])

  useLayoutEffect(() => {
    if (element) {
      const sizeString = getComputedStyle(element).borderRadius
      setBorderRadius(convertCSSUnitString(sizeString, 'px', element).number)
    }
  }, [element])

  if (!bounds) {
    return null
  }

  const { top: topRaw, left: leftRaw, height, width } = bounds

  const top = topRaw - paddingPx
  const left = leftRaw - paddingPx

  const r = Math.min(borderRadius, height / 2 + paddingPx, width / 2 + paddingPx)
  const straightWidth = Math.max(0, width + paddingPx * 2 - borderRadius * 2)
  const straightHeight = Math.max(0, height + paddingPx * 2 - borderRadius * 2)

  const clipPath =
    // A rectangle covering the entire screen
    'path(evenodd, "M0 0L3840 0 3840 2160 0 2160Z' +
    // Move to top left
    `M${left + r} ${top}` +
    // Top edge
    `h${straightWidth}` +
    // Top right arc
    (r !== 0 ? `a${r} ${r} 0 0 1 ${r} ${r}` : '') +
    // Right edge
    `v${straightHeight}` +
    // Bottom right arc
    (r !== 0 ? `a${r} ${r} 0 0 1 -${r} ${r}` : '') +
    // Bottom edge
    `h-${straightWidth}` +
    // Bottom left arc
    (r !== 0 ? `a${r} ${r} 0 0 1 -${r} -${r}` : '') +
    // Left edge
    `v-${straightHeight}` +
    // Top left arc
    (r !== 0 ? `a${r} ${r} 0 0 1 ${r} -${r}` : '') +
    'Z")'

  return (
    <Portal>
      <div
        onClick={close}
        className="absolute inset-0 h-full w-full bg-primary/25 contain-strict"
        style={{ clipPath }}
      />
    </Portal>
  )
}
