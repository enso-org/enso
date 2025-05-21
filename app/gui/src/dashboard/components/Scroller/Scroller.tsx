/** @file A component that adds scroll shadows to a container. */
import type { TestIdProps } from '#/components/AriaComponents'
import { SCROLLER_STYLES } from '#/components/Scroller/constants'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import {
  startTransition,
  useCallback,
  useRef,
  useState,
  type HTMLAttributes,
  type PropsWithChildren,
} from 'react'

/** Props for {@link Scroller}. */
export interface ScrollerProps
  extends HTMLAttributes<HTMLDivElement>,
    PropsWithChildren,
    TestIdProps,
    Omit<VariantProps<typeof SCROLLER_STYLES>, 'endHidden' | 'startHidden'> {
  readonly shadowStartClassName?: string
}

/** A component that adds scroll shadows to a container. */
export function Scroller(props: ScrollerProps) {
  const {
    className,
    shadowStartClassName,
    scrollbar = false,
    snap = false,
    variants = SCROLLER_STYLES,
    orientation = 'horizontal',
    showShadows = true,
    testId = 'scroller',
    onScroll,
    ...rest
  } = props

  const containerRef = useRef<HTMLDivElement>(null)

  const [startHidden, setStartHidden] = useState(true)
  const [endHidden, setEndHidden] = useState(true)

  const setHidden = useEventCallback((start: boolean, end: boolean) => {
    startTransition(() => {
      setStartHidden(start)
      setEndHidden(end)
    })
  })

  const [measureRef] = useMeasureCallback({
    isDisabled: !showShadows,
    onResize: () => {
      const container = containerRef.current

      if (!container) {
        return
      }

      const { isAtStart, isAtEnd } = calculateShadows(container)

      setHidden(isAtStart, isAtEnd)
    },
  })

  useEventListener(
    'scroll',
    () => {
      const container = containerRef.current

      if (!container) {
        return
      }

      const { isAtStart, isAtEnd } = calculateShadows(container)

      setHidden(isAtStart, isAtEnd)
    },
    containerRef,
    { passive: true, isDisabled: !showShadows },
  )

  const calculateShadows = useEventCallback((element: HTMLDivElement) => {
    const { scrollLeft, clientWidth, scrollTop, clientHeight, scrollWidth, scrollHeight } = element

    const scrollStart = orientation === 'horizontal' ? scrollLeft : scrollTop
    const size = orientation === 'horizontal' ? clientWidth : clientHeight
    const scrollSize = orientation === 'horizontal' ? scrollWidth : scrollHeight

    const isAtStart = scrollStart === 0
    const isAtEnd = Math.ceil(scrollStart + size) >= scrollSize

    return { isAtStart, isAtEnd }
  })

  const refCallback = useCallback(
    (el: HTMLDivElement | null) => {
      if (!el) {
        return
      }

      const { isAtStart, isAtEnd } = calculateShadows(el)

      setHidden(isAtStart, isAtEnd)
    },
    [calculateShadows, setHidden],
  )

  const styles = variants({
    scrollbar,
    snap,
    orientation,
    startHidden,
    endHidden,
    showShadows,
  })

  return (
    <div className={styles.base({ className })} data-testid={testId} {...rest}>
      <div
        ref={(el) => {
          mergeRefs(refCallback, measureRef, containerRef)(el)
        }}
        onScroll={onScroll}
        className={styles.content()}
      >
        {props.children}
      </div>

      <div aria-hidden className={styles.shadowStart({ className: shadowStartClassName })} />
      <div aria-hidden className={styles.shadowEnd()} />
    </div>
  )
}
