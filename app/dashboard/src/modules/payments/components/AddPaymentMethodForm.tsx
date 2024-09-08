/**
 * @file
 *
 * A modal for adding a payment method.
 */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeJs from '@stripe/stripe-js'
import * as reactQuery from '@tanstack/react-query'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * Props for {@link AddPaymentMethodForm}.
 */
export interface AddPaymentMethodFormProps<
  Schema extends typeof ADD_PAYMENT_METHOD_FORM_SCHEMA = typeof ADD_PAYMENT_METHOD_FORM_SCHEMA,
> {
  readonly stripeInstance: stripeJs.Stripe
  readonly elements: stripeJs.StripeElements
  readonly submitText: string
  readonly onSubmit?:
    | ((paymentMethodId: stripeJs.PaymentMethod['id']) => Promise<void> | void)
    | undefined
  readonly form?: ariaComponents.FormInstance<Schema>
}

export const ADD_PAYMENT_METHOD_FORM_SCHEMA = ariaComponents.Form.schema.object({
  card: ariaComponents.Form.schema
    .object(
      {
        complete: ariaComponents.Form.schema.boolean(),
        error: ariaComponents.Form.schema
          .object({ message: ariaComponents.Form.schema.string() })
          .nullish(),
      },
      { message: 'This field is required' },
    )
    .nullable()
    .refine(
      (data) => data?.error == null,
      (data) => ({ message: data?.error?.message ?? 'This field is required' }),
    ),
})

/**
 * A form for adding a payment method.
 */
export function AddPaymentMethodForm<
  Schema extends typeof ADD_PAYMENT_METHOD_FORM_SCHEMA = typeof ADD_PAYMENT_METHOD_FORM_SCHEMA,
>(props: AddPaymentMethodFormProps<Schema>) {
  const { stripeInstance, elements, onSubmit, submitText, form } = props

  const { getText } = text.useText()

  const [cardElement, setCardElement] = React.useState<stripeJs.StripeCardElement | null>(() =>
    elements.getElement(stripeReact.CardElement),
  )

  const dialogContext = ariaComponents.useDialogContext()

  const createPaymentMethodMutation = reactQuery.useMutation({
    mutationFn: async () => {
      if (!cardElement) {
        throw new Error('Unexpected error')
      } else {
        return stripeInstance
          .createPaymentMethod({ type: 'card', card: cardElement })
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

  // No idea if it's safe or not, but outside of the function everything is fine
  // but for some reason TypeScript fails to infer the `card` field from the schema (it should always be there)
  const formInstance = ariaComponents.Form.useForm(
    // eslint-disable-next-line no-restricted-syntax
    (form as ariaComponents.FormInstance<typeof ADD_PAYMENT_METHOD_FORM_SCHEMA> | undefined) ?? {
      schema: ADD_PAYMENT_METHOD_FORM_SCHEMA,
    },
  )

  return (
    <ariaComponents.Form
      method="dialog"
      form={formInstance}
      onSubmit={() =>
        createPaymentMethodMutation.mutateAsync().then(async ({ paymentMethod }) => {
          cardElement?.clear()
          await onSubmit?.(paymentMethod.id)
        })
      }
    >
      <ariaComponents.Form.Field name="card" fullWidth label={getText('bankCardLabel')}>
        <stripeReact.CardElement
          options={{
            classes: {
              base: 'border border-primary/15 rounded-2xl px-3 py-2 transition-[outline] w-full',
              focus: 'outline outline-2 -outline-offset-1 border-primary outline-primary',
              complete: 'border-primary outline-primary',
              invalid: 'border-danger outline-danger',
            },
          }}
          onEscape={() => dialogContext?.close()}
          onReady={(element) => {
            setCardElement(element)
          }}
          onChange={(event) => {
            if (event.error?.message != null) {
              formInstance.setError('card', { message: event.error.message })
              cardElement?.focus()
            } else {
              formInstance.clearErrors('card')
            }
            formInstance.setValue('card', event)
          }}
          onBlur={() => formInstance.trigger('card')}
        />
      </ariaComponents.Form.Field>

      <ariaComponents.Form.Submit loading={cardElement == null}>
        {submitText}
      </ariaComponents.Form.Submit>

      <ariaComponents.Form.FormError />
    </ariaComponents.Form>
  )
}
