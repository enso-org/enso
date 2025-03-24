/**
 * @file
 *
 * Scroller is a component that
 */

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  startTransition,
  useCallback,
  useRef,
  useState,
  type HTMLAttributes,
  type PropsWithChildren,
} from 'react'
import type { TestIdProps } from '../AriaComponents'

export const SCROLLER_STYLES = tv({
  base: 'relative w-auto min-w-0',
  variants: {
    scrollbar: {
      false: {
        content: 'no-scrollbar',
      },
    },
    orientation: {
      horizontal: {
        content: '',
      },
      vertical: {
        content: '',
      },
    },
    snap: {
      true: {
        content: '',
      },
      false: {
        content: '',
      },
    },
    startHidden: {
      true: {
        shadowStart: 'opacity-0',
      },
      false: {
        shadowStart: 'opacity-100',
      },
    },
    endHidden: {
      true: {
        shadowEnd: 'opacity-0',
      },
      false: {
        shadowEnd: 'opacity-100',
      },
    },
    showShadows: {
      true: {
        shadowStart: '',
        shadowEnd: '',
      },
      false: {
        shadowStart: 'hidden',
        shadowEnd: 'hidden',
      },
    },
  },

  slots: {
    content: '',
    shadowStart: 'pointer-events-none absolute from-dashboard transition-opacity',
    shadowEnd: 'pointer-events-none absolute from-dashboard transition-opacity',
  },

  compoundVariants: [
    {
      orientation: 'horizontal',
      snap: true,
      class: {
        content: 'snap-x snap-proximity',
      },
    },
    {
      orientation: 'horizontal',
      class: {
        content: 'overflow-x-auto min-w-0 max-w-full',
        shadowStart: 'top-0 bottom-0 left-0 w-10',
        shadowEnd: 'top-0 bottom-0 right-0 w-10',
      },
    },
    {
      orientation: 'horizontal',
      class: {
        content: 'overflow-x-auto min-w-0 max-w-full',
        shadowStart: 'top-0 bottom-0 left-0 w-10 bg-gradient-to-r',
        shadowEnd: 'top-0 bottom-0 right-0 w-10 bg-gradient-to-l',
      },
    },
    {
      orientation: 'vertical',
      snap: true,
      class: {
        content: 'snap-y snap-proximity',
      },
    },
    {
      orientation: 'vertical',
      class: {
        content: 'overflow-y-auto min-h-0 max-h-full',
        shadowStart: '-top-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-b',
        shadowEnd: '-bottom-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-t',
      },
    },
  ],

  defaultVariants: {
    scrollbar: false,
    snap: false,
    orientation: 'horizontal',
    showShadows: true,
    startHidden: true,
    endHidden: true,
  },
})

/**
 * Props for {@link Scroller}.
 */
export interface ScrollerProps
  extends HTMLAttributes<HTMLDivElement>,
    PropsWithChildren,
    TestIdProps,
    Omit<VariantProps<typeof SCROLLER_STYLES>, 'endHidden' | 'startHidden'> {}

/**
 * A component that adds scroll shadows to a container.
 */
export function Scroller(props: ScrollerProps) {
  const {
    className,
    scrollbar = false,
    snap = false,
    variants = SCROLLER_STYLES,
    orientation = 'horizontal',
    showShadows = true,
    testId = 'scroller',
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
        className={styles.content()}
      >
        {props.children}
      </div>

      <div aria-hidden className={styles.shadowStart()} />
      <div aria-hidden className={styles.shadowEnd()} />
    </div>
  )
}
