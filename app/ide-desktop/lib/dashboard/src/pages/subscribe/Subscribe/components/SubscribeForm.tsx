/**
 * @file
 *
 * A form for subscribing to a plan.
 */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeJs from '@stripe/stripe-js'
import * as reactQuery from '@tanstack/react-query'

import * as text from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

/**
 *
 */
export interface SubscribeFormProps {
  readonly stripe: stripeJs.Stripe
  readonly elements: stripeJs.StripeElements
  readonly onSubmit: (paymentMethodId: stripeJs.PaymentMethod['id']) => Promise<void>
}

/**
 * A form for subscribing to a plan.
 */
export function SubscribeForm(props: SubscribeFormProps) {
  const { stripe, elements, onSubmit } = props

  const { getText } = text.useText()
  const cardElement = elements.getElement(stripeReact.CardElement)
  const [cardElementState, setCardElementState] =
    React.useState<stripeJs.StripeElementChangeEvent | null>(null)

  const subscribeMutation = reactQuery.useMutation({
    mutationFn: async () => {
      if (!cardElement) {
        throw new Error('Unexpected error')
      } else {
        return stripe.createPaymentMethod({ type: 'card', card: cardElement }).then(result => {
          if (result.error) {
            throw new Error(result.error.message)
          } else {
            return result
          }
        })
      }
    },
    onSuccess: async paymentMethod => {
      await onSubmit(paymentMethod.paymentMethod.id)
      cardElement?.clear()
    },
  })

  return (
    <aria.Form
      className="flex flex-col items-start gap-2"
      onSubmit={event => {
        event.preventDefault()
        subscribeMutation.mutate()
      }}
    >
      <div className="flex w-full flex-col gap-2">
        <aria.TextField isInvalid={cardElementState?.error != null} isRequired>
          <aria.Label className="mb-1 ml-0.5 block text-sm">{getText('BankCardLabel')}</aria.Label>
          <stripeReact.CardElement
            options={{
              classes: {
                base: 'border border-gray-300 rounded-md p-3 transition-[outline]',
                empty: '',
                focus: 'outline outline-2 outline-primary',
                complete: 'border-blue-500 outline-blue-500',
                invalid: 'border-red-500 outline-red-500',
              },
            }}
            onChange={setCardElementState}
          />
          <aria.FieldError className="text-sm text-red-500">
            {cardElementState?.error?.message}
          </aria.FieldError>
        </aria.TextField>
      </div>

      {subscribeMutation.error && (
        <ariaComponents.Alert variant="error" size="medium">
          {subscribeMutation.error.message}
        </ariaComponents.Alert>
      )}

      <ariaComponents.Button
        type="submit"
        variant="submit"
        loading={subscribeMutation.isPending}
        isDisabled={
          subscribeMutation.isPending || cardElementState == null || cardElementState.error != null
        }
        className="mt-1 px-4 py-1.5 text-sm"
      >
        {getText('subscribeSubmit')}
      </ariaComponents.Button>
    </aria.Form>
  )
}
