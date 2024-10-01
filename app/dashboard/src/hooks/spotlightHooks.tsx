/** @file Hooks for showing an overlay with a cutout for a rectangular element. */
import { useEffect, useLayoutEffect, useState, type CSSProperties, type RefObject } from 'react'
import { createPortal } from 'react-dom'

import { useDimensions } from '#/hooks/dimensionsHooks'
import { convertCSSUnitString } from '#/utilities/convertCSSUnits'

/** Default padding around the spotlight element. */
const DEFAULT_PADDING_PX = 8

// eslint-disable-next-line no-restricted-syntax
const BACKGROUND_ELEMENT = document.getElementsByClassName('enso-spotlight')[0] as
  | HTMLElement
  | SVGElement
  | undefined

/** Props for {@link useSpotlight}. */
export interface SpotlightOptions {
  readonly enabled: boolean
  readonly ref: RefObject<HTMLElement | SVGElement | null>
  readonly close: () => void
  readonly backgroundElement?: HTMLElement
  readonly paddingPx?: number | undefined
}

/** A hook for showing an overlay with a cutout for a rectangular element. */
export function useSpotlight(options: SpotlightOptions) {
  const { enabled, ref, close, backgroundElement: backgroundElementRaw } = options
  const { paddingPx = DEFAULT_PADDING_PX } = options
  const backgroundElement = backgroundElementRaw ?? BACKGROUND_ELEMENT

  const spotlightElement =
    !enabled || !backgroundElement ?
      null
    : <Spotlight
        close={close}
        element={ref}
        backgroundElement={backgroundElement}
        paddingPx={paddingPx}
      />
  const style = { position: 'relative', zIndex: 3 } satisfies CSSProperties
  return { spotlightElement, props: { style } }
}

/** Props for a {@link Spotlight}. */
interface SpotlightProps {
  readonly element: RefObject<HTMLElement | SVGElement | null>
  readonly close: () => void
  readonly backgroundElement: HTMLElement | SVGElement
  readonly paddingPx?: number | undefined
}

/** A spotlight element. */
function Spotlight(props: SpotlightProps) {
  const { element, close, backgroundElement, paddingPx = 0 } = props
  const [dimensionsRef, { top: topRaw, left: leftRaw, height, width }] = useDimensions()
  const top = topRaw - paddingPx
  const left = leftRaw - paddingPx
  const [borderRadius, setBorderRadius] = useState(0)
  const r = Math.min(borderRadius, height / 2 + paddingPx, width / 2 + paddingPx)
  const straightWidth = Math.max(0, width + paddingPx * 2 - borderRadius * 2)
  const straightHeight = Math.max(0, height + paddingPx * 2 - borderRadius * 2)

  useEffect(() => {
    if (element.current) {
      dimensionsRef(element.current)
    }
  }, [dimensionsRef, element])

  useLayoutEffect(() => {
    if (element.current) {
      const sizeString = getComputedStyle(element.current).borderRadius
      setBorderRadius(convertCSSUnitString(sizeString, 'px', element.current).number)
    }
  }, [element])

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

  return createPortal(
    <div
      onClick={close}
      style={{
        position: 'absolute',
        zIndex: 2,
        height: '100vh',
        width: '100vw',
        backgroundColor: 'lch(0 0 0 / 25%)',
        clipPath,
      }}
    />,
    backgroundElement,
  )
}
