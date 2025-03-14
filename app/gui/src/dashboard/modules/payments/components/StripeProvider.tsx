/** @file A component that provides a Stripe context. */
import { Elements, ElementsConsumer } from '@stripe/react-stripe-js'
import type { Stripe, StripeElements } from '@stripe/stripe-js'
import type { ReactNode } from 'react'
import { useStripeLoader } from './hooks'

/** Props for a {@link StripeProvider}. */
export interface StripeProviderProps {
  readonly children: ReactNode | ((props: StripeProviderRenderProps) => ReactNode)
}

/** Render props for children of a {@link StripeProvider}. */
export interface StripeProviderRenderProps {
  readonly stripe: Stripe
  readonly elements: StripeElements
}

/** A component that provides a Stripe context. */
export function StripeProvider(props: StripeProviderProps) {
  const { children } = props

  const stripeInstance = useStripeLoader()

  return (
    <Elements stripe={stripeInstance.data}>
      <ElementsConsumer>
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
      </ElementsConsumer>
    </Elements>
  )
}
