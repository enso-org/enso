/** @file A modal for adding a payment method. */
import { CardElement } from '@stripe/react-stripe-js'
import type { PaymentMethod, Stripe, StripeCardElement, StripeElements } from '@stripe/stripe-js'

import { Form, useDialogContext, type FormInstance, type schema } from '#/components/AriaComponents'
import { useText, type GetText } from '#/providers/TextProvider'
import { useCreatePaymentMethodMutation } from '../api/createPaymentMethod'

/** Props for an {@link AddPaymentMethodForm}. */
export interface AddPaymentMethodFormProps<
  Schema extends ReturnType<typeof createAddPaymentMethodFormSchema> = ReturnType<
    typeof createAddPaymentMethodFormSchema
  >,
> {
  readonly stripeInstance: Stripe
  readonly elements: StripeElements
  readonly submitText: string
  readonly onSubmit?: ((paymentMethodId: PaymentMethod['id']) => Promise<void> | void) | undefined
  readonly form?: FormInstance<Schema>
}

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

/** A form for adding a payment method. */
export function AddPaymentMethodForm<
  Schema extends ReturnType<typeof createAddPaymentMethodFormSchema> = ReturnType<
    typeof createAddPaymentMethodFormSchema
  >,
>(props: AddPaymentMethodFormProps<Schema>) {
  const { stripeInstance, onSubmit, submitText, form: formRaw } = props
  const { getText } = useText()
  const dialogContext = useDialogContext()
  const createPaymentMethodMutation = useCreatePaymentMethodMutation()

  const form = Form.useForm(
    formRaw ?? {
      mode: 'onChange',
      schema: (z) => createAddPaymentMethodFormSchema(z, getText),
      onSubmit: ({ cardElement }) =>
        createPaymentMethodMutation.mutateAsync({ stripeInstance, cardElement }),
      onSubmitSuccess: ({ paymentMethod }) => onSubmit?.(paymentMethod.id),
    },
  )

  const cardElement =
    // FIXME[sb]: I do not understand why `useWatch` is not sufficient for Playwright.
    // (The value is always `undefined` with `useWatch` alone)
    // It is worth noting that E2E tests previously worked without requiring this change - as of:
    // 1500849c32f70f5f4d95240b7e31377c649dc25b
    Form.useWatch({ control: form.control, name: 'cardElement' }) ?? form.getValues().cardElement

  return (
    <Form method="dialog" form={form}>
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
            form.setValue('cardElement', element)
            form.setValue('stripeInstance', stripeInstance)
          }}
          onChange={(event) => {
            if (event.error?.message != null) {
              form.setError('card', { message: event.error.message })
              cardElement?.focus()
            } else {
              form.clearErrors('card')
            }
            form.setValue('card', event)
          }}
          onBlur={() => form.trigger('card')}
        />
      </Form.Field>

      <Form.Submit loading={cardElement == null}>{submitText}</Form.Submit>

      <Form.FormError />
    </Form>
  )
}
