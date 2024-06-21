/**
 * @file
 *
 * A modal for adding a payment method.
 */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeJs from '@stripe/stripe-js'
import * as reactQuery from '@tanstack/react-query'

import * as stripeProvider from '#/providers/StripeProvider'
import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * Props for {@link AddPaymentMethodModal}.
 */
export interface AddPaymentMethodModalProps {
  readonly title: string
  readonly submitText: string
  readonly onSubmit: (paymentMethodId: stripeJs.PaymentMethod['id']) => Promise<void> | void
}

/**
 * A modal for adding a payment method.
 */
export default function AddPaymentMethodModal(props: AddPaymentMethodModalProps) {
  const { title, onSubmit, submitText } = props

  return (
    <ariaComponents.Dialog title={title}>
      <stripeProvider.StripeProvider>
        {({ stripe, elements }) => (
          <AddPaymentMethodForm
            onSubmit={onSubmit}
            elements={elements}
            stripeInstance={stripe}
            submitText={submitText}
          />
        )}
      </stripeProvider.StripeProvider>
    </ariaComponents.Dialog>
  )
}

/**
 * Props for {@link AddPaymentMethodForm}.
 */
export interface AddPaymentMethodFormProps {
  readonly stripeInstance: stripeJs.Stripe
  readonly elements: stripeJs.StripeElements
  readonly submitText: string
  readonly onSubmit?: (paymentMethodId: stripeJs.PaymentMethod['id']) => Promise<void> | void
}

/**
 * A form for adding a payment method.
 */
export function AddPaymentMethodForm(props: AddPaymentMethodFormProps) {
  const { stripeInstance, elements, onSubmit, submitText } = props

  const { getText } = text.useText()

  const [cardElement, setCardElement] = React.useState<stripeJs.StripeCardElement | null>(() =>
    elements.getElement(stripeReact.CardElement)
  )

  const dialogContext = ariaComponents.useDialogContext()

  const subscribeMutation = reactQuery.useMutation({
    mutationFn: async () => {
      if (!cardElement) {
        throw new Error('Unexpected error')
      } else {
        return stripeInstance
          .createPaymentMethod({ type: 'card', card: cardElement })
          .then(result => {
            if (result.error) {
              throw new Error(result.error.message)
            } else {
              return result
            }
          })
      }
    },
    onSuccess: async paymentMethod => {
      await onSubmit?.(paymentMethod.paymentMethod.id)
      cardElement?.clear()
    },
  })

  const form = ariaComponents.Form.useForm({
    schema: ariaComponents.Form.schema.object({
      card: ariaComponents.Form.schema
        .object(
          {
            complete: ariaComponents.Form.schema.boolean(),
            error: ariaComponents.Form.schema
              .object({ message: ariaComponents.Form.schema.string() })
              .nullish(),
          },
          { message: getText('arbitraryFieldRequired') }
        )
        .nullable()
        .refine(
          data => data?.error == null,
          data => ({ message: data?.error?.message ?? getText('arbitraryFieldRequired') })
        ),
    }),
  })

  return (
    <ariaComponents.Form
      method="dialog"
      form={form}
      onSubmit={() => subscribeMutation.mutateAsync()}
    >
      <ariaComponents.Form.Field name="card" fullWidth label={getText('bankCardLabel')}>
        <stripeReact.CardElement
          options={{
            classes: {
              empty: '',
              base: 'border border-gray-300 rounded-md p-3 transition-[outline] w-full',
              focus: 'outline outline-2 outline-primary',
              complete: 'border-blue-500 outline-blue-500',
              invalid: 'border-red-500 outline-red-500',
            },
          }}
          onEscape={() => dialogContext?.close()}
          onReady={element => {
            setCardElement(element)
            element.focus()
          }}
          onChange={event => {
            if (event.error?.message != null) {
              form.setError('card', { message: event.error.message })
            } else {
              form.clearErrors('card')
            }
            form.setValue('card', event)
          }}
          onBlur={() => form.trigger('card')}
        />
      </ariaComponents.Form.Field>

      <ariaComponents.Form.FormError />

      <ariaComponents.Form.Submit loading={cardElement == null}>
        {submitText}
      </ariaComponents.Form.Submit>
    </ariaComponents.Form>
  )
}
