/**
 * @file
 *
 * This file contains the hook to get the toasts from the toast context
 */
import * as sonner from 'sonner'

/**
 * Hook to get the toasts from the toast context
 */
export function useToasts() {
  const { toasts } = sonner.useSonner()

  return {
    toasts,
  } as const
}
