/** @file Mock for `@stripe/stripe-js` */
import type { Stripe } from '@stripe/stripe-js'

export const loadStripe = (): Promise<Stripe> =>
  Promise.resolve({
    createPaymentMethod: () =>
      Promise.resolve({
        paymentMethod: {
          id: '',
          object: 'payment_method',
          // eslint-disable-next-line camelcase
          billing_details: {
            address: null,
            email: null,
            name: null,
            phone: null,
          },
          created: Number(new Date()) / 1_000,
          customer: null,
          livemode: true,
          metadata: {},
          type: '',
        },
        error: undefined,
      }),
  } satisfies Partial<Stripe> as Partial<Stripe> as Stripe)
