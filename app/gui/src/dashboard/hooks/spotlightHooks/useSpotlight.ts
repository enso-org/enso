/** @file Hooks for showing an overlay with a cutout for a rectangular element. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { createElement, useState, type CSSProperties } from 'react'
import { Spotlight } from './Spotlight'

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
  readonly close: () => void
  readonly backgroundElement?: HTMLElement
  readonly paddingPx?: number | undefined
}

/** A hook for showing an overlay with a cutout for a rectangular element. */
export function useSpotlight(options: SpotlightOptions) {
  const { enabled, close, backgroundElement: backgroundElementRaw } = options
  const { paddingPx = DEFAULT_PADDING_PX } = options
  const backgroundElement = backgroundElementRaw ?? BACKGROUND_ELEMENT

  const [refElement, setRefElement] = useState<HTMLElement | SVGElement | null>(null)

  const refCallback = useEventCallback((node: HTMLElement | SVGElement | null) => {
    if (node) {
      setRefElement(node)
    } else {
      setRefElement(null)
    }
  })

  const spotlightElement =
    !enabled || !backgroundElement ?
      null
    : createElement(Spotlight, {
        close,
        element: refElement,
        backgroundElement,
        paddingPx,
      })
  const style = { position: 'relative', zIndex: 3 } satisfies CSSProperties
  return { spotlightElement, props: { style, ref: refCallback } }
}
