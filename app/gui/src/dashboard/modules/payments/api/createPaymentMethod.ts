/**
 * @file
 *
 * API for creating a payment method.
 */
import type { Stripe, StripeCardElement } from '@stripe/stripe-js'
import { useMutation } from '@tanstack/react-query'

/** Parameters for the `createPaymentMethod` mutation. */
export interface CreatePaymentMethodMutationParams {
  readonly cardElement?: StripeCardElement | null | undefined
  readonly stripeInstance: Stripe
}

/** Hook for creating a payment method. */
export function useCreatePaymentMethodMutation() {
  return useMutation({
    mutationFn: async (params: CreatePaymentMethodMutationParams) => {
      if (!params.cardElement) {
        throw new Error('Unexpected error')
      } else {
        return params.stripeInstance
          .createPaymentMethod({ type: 'card', card: params.cardElement })
          .then((result) => {
            if (result.error) {
              throw new Error(result.error.message)
            } else {
              return result
            }
          })
      }
    },
  })
}
