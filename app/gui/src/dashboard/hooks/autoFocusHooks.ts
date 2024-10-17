/**
 * @file
 * Hooks for automatically focusing elements.
 */

import { useInteractOutside } from '#/components/aria'
import { useEffect, useRef } from 'react'
import { useEventCallback } from './eventCallbackHooks'

/** Props for the {@link useAutoFocus} hook. */
export interface UseAutoFocusProps {
  readonly ref: React.RefObject<HTMLElement>
  readonly disabled?: boolean | undefined
}

const FOCUS_TRYOUT_DELAY = 300
const FOCUS_DELAY = 100

/**
 * Hook for automatically focusing an element.
 * Tries to focus the element for a period of time, and if it fails, it will
 * try again in a loop.
 * If user interacts with the page, the focus will be cancelled.
 */
export function useAutoFocus(props: UseAutoFocusProps) {
  const { ref, disabled = false } = props

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
      const element = ref.current
      if (element != null) {
        element.focus()
      }

      scheduledFocusRef.current = null
    }, FOCUS_DELAY)

    return () => {
      clearScheduledFocus()
    }
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
  }, [disabled, scheduleFocus, clearScheduledFocus])

  useEffect(() => {
    if (disabled) {
      return
    }

    const body = document.body

    const handleFocus = () => {
      const activeElement = document.activeElement
      const element = ref instanceof HTMLElement ? ref : ref.current

      if (element == null) {
        return
      }

      if (activeElement !== element && shouldForceFocus.current) {
        scheduleFocus()
      }
    }

    const id = setTimeout(() => {
      shouldForceFocus.current = false
      clearScheduledFocus()
    }, FOCUS_TRYOUT_DELAY)

    body.addEventListener('focus', handleFocus, { capture: true, passive: true })

    return () => {
      body.removeEventListener('focus', handleFocus)
      clearTimeout(id)
    }
  }, [disabled, scheduleFocus, ref, clearScheduledFocus])
}
