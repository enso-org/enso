/** @file A modal for adding a payment method. */
import { CardElement } from '@stripe/react-stripe-js'
import type { PaymentMethod, Stripe, StripeCardElement, StripeElements } from '@stripe/stripe-js'

import { Form, type FormInstance, useDialogContext } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { useCreatePaymentMethodMutation } from '../api/createPaymentMethod'

/** Props for an {@link AddPaymentMethodForm}. */
export interface AddPaymentMethodFormProps<
  Schema extends typeof ADD_PAYMENT_METHOD_FORM_SCHEMA = typeof ADD_PAYMENT_METHOD_FORM_SCHEMA,
> {
  readonly stripeInstance: Stripe
  readonly elements: StripeElements
  readonly submitText: string
  readonly onSubmit?: ((paymentMethodId: PaymentMethod['id']) => Promise<void> | void) | undefined
  readonly form?: FormInstance<Schema>
}

export const ADD_PAYMENT_METHOD_FORM_SCHEMA = Form.schema.object({
  card: Form.schema
    .object(
      {
        complete: Form.schema.boolean(),
        error: Form.schema.object({ message: Form.schema.string() }).nullish(),
      },
      { message: 'This field is required' },
    )
    .nullable()
    .refine(
      (data) => data?.error == null,
      (data) => ({ message: data?.error?.message ?? 'This field is required' }),
    ),
  cardElement: Form.schema.custom<StripeCardElement | null>(),
  stripeInstance: Form.schema.custom<Stripe>(),
})

/**
 * A form for adding a payment method.
 */
export function AddPaymentMethodForm<
  Schema extends typeof ADD_PAYMENT_METHOD_FORM_SCHEMA = typeof ADD_PAYMENT_METHOD_FORM_SCHEMA,
>(props: AddPaymentMethodFormProps<Schema>) {
  const { stripeInstance, onSubmit, submitText, form } = props

  const { getText } = useText()

  const dialogContext = useDialogContext()

  const createPaymentMethodMutation = useCreatePaymentMethodMutation()

  // No idea if it's safe or not, but outside of the function everything is fine
  // but for some reason TypeScript fails to infer the `card` field from the schema (it should always be there)
  // eslint-disable-next-line no-restricted-syntax
  const formInstance = Form.useForm(
    form ?? {
      schema: ADD_PAYMENT_METHOD_FORM_SCHEMA,
      onSubmit: ({ cardElement }) =>
        createPaymentMethodMutation.mutateAsync({ stripeInstance, cardElement }),
      onSubmitSuccess: ({ paymentMethod }) => onSubmit?.(paymentMethod.id),
    },
  ) as unknown as FormInstance<typeof ADD_PAYMENT_METHOD_FORM_SCHEMA>

  const cardElement = Form.useWatch({ control: formInstance.control, name: 'cardElement' })

  return (
    <Form method="dialog" form={formInstance}>
      <Form.Field name="card" fullWidth label={getText('bankCardLabel')}>
        <CardElement
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
      </Form.Field>

      <Form.Submit loading={cardElement == null}>{submitText}</Form.Submit>

      <Form.FormError />
    </Form>
  )
}
