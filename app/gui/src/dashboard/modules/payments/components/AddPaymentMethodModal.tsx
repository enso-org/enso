/**
 * @file
 *
 * A modal for adding a payment method.
 */
import * as React from 'react'

import type * as stripeJs from '@stripe/stripe-js'

import * as ariaComponents from '#/components/AriaComponents'

import { AddPaymentMethodForm } from './AddPaymentMethodForm'
import { StripeProvider } from './StripeProvider'

/** Props for {@link AddPaymentMethodModal}. */
export interface AddPaymentMethodModalProps {
  readonly title: string
  readonly submitText: string
  readonly onSubmit: (paymentMethodId: stripeJs.PaymentMethod['id']) => Promise<void> | void
}

/** A modal for adding a payment method. */
export default function AddPaymentMethodModal(props: AddPaymentMethodModalProps) {
  const { title, onSubmit, submitText } = props

  return (
    <ariaComponents.Dialog title={title}>
      <StripeProvider>
        {({ stripe, elements }) => (
          <AddPaymentMethodForm
            onSubmit={onSubmit}
            elements={elements}
            stripeInstance={stripe}
            submitText={submitText}
          />
        )}
      </StripeProvider>
    </ariaComponents.Dialog>
  )
}
