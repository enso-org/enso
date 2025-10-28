/** @file A component that adds scroll shadows to a container. */
import type { TestIdProps } from '#/components/types'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  forwardRef,
  startTransition,
  useCallback,
  useRef,
  useState,
  type ForwardedRef,
  type HTMLAttributes,
  type PropsWithChildren,
} from 'react'

// eslint-disable-next-line react-refresh/only-export-components
export const SCROLLER_STYLES = tv({
  base: 'relative w-auto min-w-0',
  variants: {
    scrollbar: {
      false: {
        content: 'no-scrollbar',
      },
    },
    background: {
      primary: {
        shadowStart: 'from-dashboard',
        shadowEnd: 'from-dashboard',
      },
      secondary: {
        shadowStart: 'from-background/80',
        shadowEnd: 'from-background/80',
      },
      white: {
        shadowStart: 'from-white',
        shadowEnd: 'from-white',
      },
    },
    orientation: {
      horizontal: {
        content: 'overflow-x-auto min-w-0 max-w-full',
        shadowStart: 'top-0 bottom-0 left-0 w-10 bg-gradient-to-r',
        shadowEnd: 'top-0 bottom-0 right-0 w-10 bg-gradient-to-l',
      },
      vertical: {
        content: 'overflow-y-auto min-h-0 max-h-full',
        shadowStart: '-top-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-b',
        shadowEnd: '-bottom-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-t',
      },
    },
    fullSize: { true: '' },
    snap: { true: { content: 'snap-proximity' } },
    startHidden: {
      true: { shadowStart: 'opacity-0' },
      false: { shadowStart: 'opacity-100' },
    },
    endHidden: {
      true: { shadowEnd: 'opacity-0' },
      false: { shadowEnd: 'opacity-100' },
    },
    showShadows: {
      true: '',
      false: { shadowStart: 'hidden', shadowEnd: 'hidden' },
    },
  },

  slots: {
    content: '',
    shadowStart: 'pointer-events-none absolute transition-opacity',
    shadowEnd: 'pointer-events-none absolute transition-opacity',
  },

  compoundVariants: [
    {
      orientation: 'horizontal',
      snap: true,
      class: { content: 'snap-x' },
    },
    {
      orientation: 'vertical',
      snap: true,
      class: { content: 'snap-y' },
    },
    {
      orientation: 'horizontal',
      fullSize: true,
      class: { content: 'min-w-full' },
    },
    {
      orientation: 'vertical',
      fullSize: true,
      class: { content: 'min-h-full' },
    },
  ],

  defaultVariants: {
    scrollbar: false,
    snap: false,
    orientation: 'horizontal',
    showShadows: true,
    startHidden: true,
    endHidden: true,
    background: 'primary',
    fullSize: false,
  },
})

/** Props for {@link Scroller}. */
export interface ScrollerProps
  extends HTMLAttributes<HTMLDivElement>,
    PropsWithChildren,
    TestIdProps,
    Omit<VariantProps<typeof SCROLLER_STYLES>, 'endHidden' | 'startHidden'> {
  readonly shadowStartClassName?: string
}

/** A component that adds scroll shadows to a container. */
export const Scroller = forwardRef(function Scroller(
  props: ScrollerProps,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const {
    className,
    shadowStartClassName,
    scrollbar = false,
    snap = false,
    variants = SCROLLER_STYLES,
    orientation = 'horizontal',
    showShadows = true,
    testId,
    onScroll,
    background = 'primary',
    fullSize,
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

  const calculateShadows = useEventCallback((element: HTMLDivElement) => {
    const { scrollLeft, clientWidth, scrollTop, clientHeight, scrollWidth, scrollHeight } = element

    const scrollStart = orientation === 'horizontal' ? scrollLeft : scrollTop
    const size = orientation === 'horizontal' ? clientWidth : clientHeight
    const scrollSize = orientation === 'horizontal' ? scrollWidth : scrollHeight

    const isAtStart = scrollStart === 0
    const isAtEnd = Math.ceil(scrollStart + size) >= scrollSize

    return { isAtStart, isAtEnd }
  })

  const updateShadows = useCallback(
    (el: HTMLDivElement | null) => {
      if (!el) return
      const { isAtStart, isAtEnd } = calculateShadows(el)
      setHidden(isAtStart, isAtEnd)
    },
    [calculateShadows, setHidden],
  )

  const [measureRef] = useMeasureCallback({
    isDisabled: !showShadows,
    onResize: () => {
      updateShadows(containerRef.current)
    },
  })

  useEventListener(
    'scroll',
    () => {
      updateShadows(containerRef.current)
    },
    containerRef,
    { passive: true, isDisabled: !showShadows },
  )

  const styles = variants({
    scrollbar,
    snap,
    orientation,
    startHidden,
    endHidden,
    showShadows,
    background,
    fullSize,
  })

  return (
    <div className={styles.base({ className })} data-testid={testId} {...rest}>
      <div
        ref={(el) => {
          mergeRefs(ref, updateShadows, measureRef, containerRef)(el)
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
})
