/** @file A React hook to prevent navigation. */
import { useEffect } from 'react'

/** Options for {@link usePreventNavigation}. */
export interface PreventNavigationOptions {
  readonly isEnabled?: boolean
}

/** Prevent navigating away from a page. */
export function usePreventNavigation(options: PreventNavigationOptions) {
  const { isEnabled = true } = options

  useEffect(() => {
    if (isEnabled) {
      const onClose = (event: Event) => {
        event.preventDefault()
      }
      window.addEventListener('close', onClose)
      return () => {
        window.removeEventListener('close', onClose)
      }
    }
  }, [isEnabled])
}
