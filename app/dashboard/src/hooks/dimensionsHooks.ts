/** @file Hooks for reactively watching node dimensions. */

import { useCallback, useLayoutEffect, useState } from 'react'

/** Dimensions object for {@link useDimensions}. */
interface DimensionObject {
  readonly width: number
  readonly height: number
  readonly top: number
  readonly left: number
  readonly x: number
  readonly y: number
  readonly right: number
  readonly bottom: number
}

/** Arguments for {@link useDimensions}. */
interface UseDimensionsArgs {
  readonly liveMeasure?: boolean
}

/** Turn a bounding box to a {@link DimensionObject}. */
function getDimensionObject(node: HTMLElement | SVGElement): DimensionObject {
  const rect = node.getBoundingClientRect()

  return {
    width: rect.width,
    height: rect.height,
    // eslint-disable-next-line no-restricted-syntax
    top: 'y' in rect ? rect.y : (rect as { top: number }).top,
    // eslint-disable-next-line no-restricted-syntax
    left: 'x' in rect ? rect.x : (rect as { left: number }).left,
    // eslint-disable-next-line no-restricted-syntax
    x: 'x' in rect ? rect.x : (rect as { left: number }).left,
    // eslint-disable-next-line no-restricted-syntax
    y: 'y' in rect ? rect.y : (rect as { top: number }).top,
    right: rect.right,
    bottom: rect.bottom,
  }
}

/** Reactively watch dimensions of a node. */
export function useDimensions({ liveMeasure = true }: UseDimensionsArgs = {}): [
  ref: (node: HTMLElement | SVGElement | null) => void,
  dimensions: DimensionObject,
  element: HTMLElement | SVGElement | null,
] {
  const [dimensions, setDimensions] = useState({
    width: 0,
    height: 0,
    top: 0,
    left: 0,
    x: 0,
    y: 0,
    right: 0,
    bottom: 0,
  })
  const [node, setNode] = useState<HTMLElement | SVGElement | null>(null)

  const ref = useCallback((newNode: HTMLElement | SVGElement | null) => {
    setNode(newNode)
  }, [])

  useLayoutEffect(() => {
    if (node) {
      const measure = () => {
        requestAnimationFrame(() => {
          setDimensions(getDimensionObject(node))
        })
      }
      measure()

      if (liveMeasure) {
        const resizeObserver = new ResizeObserver(measure)
        window.addEventListener('resize', measure)
        window.addEventListener('scroll', measure)
        resizeObserver.observe(node)

        return () => {
          window.removeEventListener('resize', measure)
          window.removeEventListener('scroll', measure)
          resizeObserver.unobserve(node)
        }
      }
    }
  }, [liveMeasure, node])

  return [ref, dimensions, node]
}
