/**
 * @file
 * Hooks for automatically focusing elements.
 */

import { useInteractOutside } from '#/components/aria'
import { useEffect, useRef } from 'react'
import { useEventCallback } from './eventCallbackHooks'
import { useEventListener } from './eventListenerHooks'
import { useTimeoutCallback } from './timeoutHooks'
import { useUnmount } from './unmountHooks'

/** Props for the {@link useAutoFocus} hook. */
export interface UseAutoFocusProps {
  readonly ref: React.RefObject<HTMLElement>
  readonly disabled?: boolean | undefined
  /**
   * Called when the element is focused.
   * MAY be called multiple times, in case
   * if the element struggles to fight for focus.
   */
  readonly onFocused?: () => void
}

const FOCUS_TRYOUT_DELAY = 1_500
const FOCUS_DELAY = 100

/**
 * Hook for automatically focusing an element.
 * Tries to focus the element for a period of time, and if it fails, it will
 * try again in a loop.
 * If user interacts with the page, the focus will be cancelled.
 */
export function useAutoFocus(props: UseAutoFocusProps) {
  const { ref, disabled = false, onFocused } = props

  const shouldForceFocus = useRef(false)
  const scheduledFocusRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  useInteractOutside({
    ref,
    onInteractOutside: () => {
      // If the user clicks outside of the element, we should not force focus.
      shouldForceFocus.current = false
      clearScheduledFocus()
    },
  })

  const scheduleFocus = useEventCallback(() => {
    clearScheduledFocus()

    scheduledFocusRef.current = setTimeout(() => {
      ref.current?.focus()

      onFocused?.()

      scheduledFocusRef.current = null
    }, FOCUS_DELAY)

    return clearScheduledFocus
  })

  const clearScheduledFocus = useEventCallback(() => {
    if (scheduledFocusRef.current != null) {
      clearTimeout(scheduledFocusRef.current)
      scheduledFocusRef.current = null
    }
  })

  useEffect(() => {
    if (!disabled) {
      shouldForceFocus.current = true
    }
  }, [disabled])

  useEffect(() => {
    if (!disabled && shouldForceFocus.current) {
      return scheduleFocus()
    }
  }, [disabled, scheduleFocus])

  useEventListener(
    'focus',
    () => {
      const activeElement = document.activeElement
      const element = ref instanceof HTMLElement ? ref : ref.current

      if (element == null) {
        return
      }

      if (activeElement !== element && shouldForceFocus.current) {
        scheduleFocus()
      }
    },
    document.body,
    { isDisabled: disabled, capture: true, passive: true },
  )

  const [, stop] = useTimeoutCallback({
    callback: () => {
      shouldForceFocus.current = false
      clearScheduledFocus()
    },
    ms: FOCUS_TRYOUT_DELAY,
    isDisabled: disabled,
  })

  useUnmount(stop)
}
