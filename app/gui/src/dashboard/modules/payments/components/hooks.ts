/** @file Hooks for Stripe. */
import { useStripe as originalUseStripe, useElements } from '@stripe/react-stripe-js'
import { useSuspenseQuery } from '@tanstack/react-query'
import invariant from 'tiny-invariant'
import { stripeQueryOptions } from './utilities'

/** Hook that gets the Stripe instance and elements from the Stripe context. */
export function useStripe() {
  const stripeInstance = originalUseStripe()
  const elements = useElements()

  invariant(
    stripeInstance != null && elements != null,
    'Stripe instance not found. Make sure you are using the `StripeProvider` component.',
  )

  return { stripe: stripeInstance, elements }
}

/**
 * Hook that loads the Stripe instance using React Suspense.
 * @returns The Stripe instance.
 */
export function useStripeLoader() {
  return useSuspenseQuery(stripeQueryOptions())
}
