/**
 * @file
 *
 * A component that provides a Stripe context.
 */

import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeTypes from '@stripe/stripe-js'
import * as stripe from '@stripe/stripe-js/pure'
import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

/** Props for a {@link StripeProvider}. */
export interface StripeProviderProps {
  readonly children: React.ReactNode | ((props: StripeProviderRenderProps) => React.ReactNode)
}

/** Render props for children of a {@link StripeProvider}. */
export interface StripeProviderRenderProps {
  readonly stripe: stripeTypes.Stripe
  readonly elements: stripeTypes.StripeElements
}

export const stripeQuery = reactQuery.queryOptions({
  queryKey: ['stripe', process.env.ENSO_CLOUD_STRIPE_KEY] as const,
  staleTime: Infinity,
  gcTime: Infinity,
  meta: { persist: false },
  queryFn: async ({ queryKey }) => {
    const stripeKey = queryKey[1]

    if (stripeKey == null) {
      throw new Error('Stripe key not found')
    } else {
      return stripe.loadStripe(stripeKey).then((maybeStripeInstance) => {
        if (maybeStripeInstance == null) {
          throw new Error('Stripe instance not found')
        } else {
          return maybeStripeInstance
        }
      })
    }
  },
})

/** A component that provides a Stripe context. */
export function StripeProvider(props: StripeProviderProps) {
  const { children } = props

  const stripeInstance = useStripeLoader()

  return (
    <stripeReact.Elements stripe={stripeInstance.data}>
      <stripeReact.ElementsConsumer>
        {({ elements }) => {
          if (elements == null) {
            // This should never happen since we always pass the `stripe` instance to the `Elements` component
            // instead of passing a promise that resolves to the `stripe` instance.
            // and the fetching is handled by the `<Suspense />` component.
            // This is just a safeguard.
            return null
          } else {
            return typeof children === 'function' ?
                children({ stripe: stripeInstance.data, elements })
              : children
          }
        }}
      </stripeReact.ElementsConsumer>
    </stripeReact.Elements>
  )
}

/** Hook that gets the Stripe instance and elements from the Stripe context. */
export function useStripe() {
  const stripeInstance = stripeReact.useStripe()
  const elements = stripeReact.useElements()

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
  return reactQuery.useSuspenseQuery(stripeQuery)
}
