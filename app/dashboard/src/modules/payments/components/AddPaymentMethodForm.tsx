/**
 * @file
 *
 * A modal for adding a payment method.
 */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeJs from '@stripe/stripe-js'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import { useCreatePaymentMethodMutation } from '../api/createPaymentMethod'

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
  cardElement: ariaComponents.Form.schema.custom<stripeJs.StripeCardElement | null>(),
  stripeInstance: ariaComponents.Form.schema.custom<stripeJs.Stripe>(),
})

/**
 * A form for adding a payment method.
 */
export function AddPaymentMethodForm<
  Schema extends typeof ADD_PAYMENT_METHOD_FORM_SCHEMA = typeof ADD_PAYMENT_METHOD_FORM_SCHEMA,
>(props: AddPaymentMethodFormProps<Schema>) {
  const { stripeInstance, onSubmit, submitText, form } = props

  const { getText } = text.useText()

  const dialogContext = ariaComponents.useDialogContext()

  const createPaymentMethodMutation = useCreatePaymentMethodMutation()

  // No idea if it's safe or not, but outside of the function everything is fine
  // but for some reason TypeScript fails to infer the `card` field from the schema (it should always be there)
  // eslint-disable-next-line no-restricted-syntax
  const formInstance = ariaComponents.Form.useForm(
    form ?? {
      schema: ADD_PAYMENT_METHOD_FORM_SCHEMA,
      onSubmit: ({ cardElement }) =>
        createPaymentMethodMutation.mutateAsync({ stripeInstance, cardElement }),
      onSubmitSuccess: ({ paymentMethod }) => onSubmit?.(paymentMethod.id),
    },
  ) as unknown as ariaComponents.FormInstance<typeof ADD_PAYMENT_METHOD_FORM_SCHEMA>

  const cardElement = ariaComponents.Form.useWatch({
    control: formInstance.control,
    name: 'cardElement',
  })

  return (
    <ariaComponents.Form method="dialog" form={formInstance}>
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
            formInstance.setValue('cardElement', element)
            formInstance.setValue('stripeInstance', stripeInstance)
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
