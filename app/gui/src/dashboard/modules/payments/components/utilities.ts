/** @file Utility functions for Stripe. */
import { OfflineError } from '#/utilities/error'
import { loadStripe } from '@stripe/stripe-js/pure'
import { onlineManager, queryOptions } from '@tanstack/react-query'

/** Creates options for querying stripe instance. */
export function stripeQueryOptions() {
  return queryOptions({
    queryKey: ['stripe', $config.STRIPE_KEY] as const,
    staleTime: Infinity,
    gcTime: Infinity,
    meta: { persist: false },
    queryFn: async ({ queryKey }) => {
      const isOnline = onlineManager.isOnline()
      const stripeKey = queryKey[1]

      if (stripeKey == null) {
        throw new Error('Stripe key not found')
      }

      if (!isOnline) {
        throw new OfflineError()
      }

      const maybeStripeInstance = await loadStripe(stripeKey)

      if (maybeStripeInstance == null) {
        throw new Error('Stripe instance not found')
      }

      return maybeStripeInstance
    },
  })
}
