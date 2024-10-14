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
  readonly observePosition?: boolean
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
export function useDimensions({
  liveMeasure = true,
  observePosition = true,
}: UseDimensionsArgs = {}): [
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

      // See https://stackoverflow.com/a/74481932 for the `IntersectionObserver` code.
      if (liveMeasure) {
        const resizeObserver = new ResizeObserver(measure)
        const child = !observePosition ? null : node.appendChild(document.createElement('div'))
        const updateChildPosition = (boundingBox = child?.getBoundingClientRect()) => {
          if (child && boundingBox) {
            const wasChanged = boundingBox.left !== -1 || boundingBox.top !== -1
            child.style.marginLeft = `${parseFloat(child.style.marginLeft || '0') - boundingBox.left - 1}px`
            child.style.marginTop = `${parseFloat(child.style.marginTop || '0') - boundingBox.top - 1}px`
            if (wasChanged) {
              // On Firefox, `IntersectionObserver`s fire inconsistently when an element's
              // position is changed too quickly.
              requestAnimationFrame(() => {
                updateChildPosition()
                measure()
              })
            }
          }
        }
        if (child) {
          child.classList.add('__use-dimensions-observer')
          child.style.position = 'fixed'
          child.style.pointerEvents = 'none'
          child.style.height = '2px'
          child.style.width = '2px'
          updateChildPosition()
        }
        const intersectionObserver =
          !child ? null : (
            new IntersectionObserver(
              (entries) => {
                if (entries[0]) {
                  measure()
                  updateChildPosition() // entries[0].boundingClientRect)
                }
              },
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              { threshold: [0, 0.125, 0.375, 0.625, 0.875, 1] },
            )
          )
        window.addEventListener('resize', measure)
        window.addEventListener('scroll', measure)
        resizeObserver.observe(node)
        if (child) {
          intersectionObserver?.observe(child)
        }

        return () => {
          child?.remove()
          window.removeEventListener('resize', measure)
          window.removeEventListener('scroll', measure)
          resizeObserver.disconnect()
          intersectionObserver?.disconnect()
        }
      }
    }
  }, [liveMeasure, node, observePosition])

  return [ref, dimensions, node]
}
