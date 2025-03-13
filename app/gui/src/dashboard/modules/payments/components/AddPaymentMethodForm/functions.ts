/** @file Functions for AddPaymentMethodForm. */
import type { schema } from '#/components/AriaComponents'
import type { GetText } from '#/providers/TextProvider'
import type { Stripe, StripeCardElement } from '@stripe/stripe-js'

/** The validation schema for this form. */
export function createAddPaymentMethodFormSchema(z: typeof schema, getText: GetText) {
  return z.object({
    card: z
      .object(
        {
          complete: z.boolean(),
          error: z.object({ message: z.string() }).nullish(),
        },
        { message: getText('arbitraryFieldRequired') },
      )
      .nullable()
      .refine(
        (data) => data?.error == null,
        (data) => ({ message: data?.error?.message ?? getText('arbitraryFieldRequired') }),
      ),
    cardElement: z.custom<StripeCardElement | null | undefined>(),
    stripeInstance: z.custom<Stripe>(),
  })
}
