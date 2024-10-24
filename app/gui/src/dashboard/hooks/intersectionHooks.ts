/** @file Track changes in intersection ratio between an element and one of its ancestors. */
import * as React from 'react'

// ============================
// === useIntersectionRatio ===
// ============================

export function useIntersectionRatio(
  rootRef: Readonly<React.MutableRefObject<HTMLDivElement | null>> | null,
  targetRef: Readonly<React.MutableRefObject<HTMLElement | SVGElement | null>>,
  threshold: number[] | number,
): number
export function useIntersectionRatio<T>(
  rootRef: Readonly<React.MutableRefObject<HTMLDivElement | null>> | null,
  targetRef: Readonly<React.MutableRefObject<HTMLElement | SVGElement | null>>,
  threshold: number[] | number,
  // Undefined MUST be excluded due to how the fallback value works when a `transform` function
  // is not passed in.
  transform: (ratio: number) => Exclude<T, undefined>,
  initialValue: Exclude<T, undefined>,
): T
/**
 * Track changes in intersection ratio between an element and one of its ancestors.
 *
 * Note that if `threshold` is an array, it MUST be memoized.
 * Similarly, `rootRef` and `targetRef` MUST be stable across renders.
 */
export function useIntersectionRatio<T>(
  rootRef: Readonly<React.MutableRefObject<HTMLDivElement | null>> | null,
  targetRef: Readonly<React.MutableRefObject<HTMLElement | SVGElement | null>>,
  threshold: number[] | number,
  transform?: (ratio: number) => T,
  initialValue?: T,
) {
  // `initialValue` is guaranteed to be the right type by the overloads.
  // eslint-disable-next-line no-restricted-syntax
  const [value, setValue] = React.useState((initialValue === undefined ? 0 : initialValue) as T)
  // eslint-disable-next-line no-restricted-syntax
  const transformRef = React.useRef(transform ?? ((ratio: number) => ratio as never))
  if (transform) {
    transformRef.current = transform
  }

  React.useEffect(() => {
    const root = rootRef?.current ?? document.body
    const mainDropzone = targetRef.current
    if (mainDropzone != null) {
      const intersectionObserver = new IntersectionObserver(
        (entries) => {
          for (const entry of entries) {
            React.startTransition(() => {
              setValue(transformRef.current(entry.intersectionRatio))
            })
          }
        },
        { root, threshold },
      )
      intersectionObserver.observe(mainDropzone)

      const recomputeIntersectionRatio = () => {
        const rootRect = root.getBoundingClientRect()
        const dropzoneRect = mainDropzone.getBoundingClientRect()
        const intersectionX = Math.max(rootRect.x, dropzoneRect.x)
        const intersectionY = Math.max(rootRect.y, dropzoneRect.y)
        const intersectionRect = new DOMRect(
          intersectionX,
          intersectionY,
          Math.min(rootRect.right, dropzoneRect.right) - intersectionX,
          Math.min(rootRect.bottom, dropzoneRect.bottom) - intersectionY,
        )
        const dropzoneArea = dropzoneRect.width * dropzoneRect.height
        const intersectionArea = intersectionRect.width * intersectionRect.height
        const intersectionRatio = Math.max(0, dropzoneArea / intersectionArea)
        React.startTransition(() => {
          setValue(transformRef.current(intersectionRatio))
        })
      }
      recomputeIntersectionRatio()
      const resizeObserver = new ResizeObserver(() => {
        recomputeIntersectionRatio()
      })
      resizeObserver.observe(root)

      return () => {
        intersectionObserver.disconnect()
        resizeObserver.disconnect()
      }
    } else {
      return
    }
  }, [targetRef, rootRef, threshold])

  return value
}
