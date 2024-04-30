import * as React from 'react'

import Edit from 'enso-assets/pen.svg'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 *
 */
export interface PaymentMethodProps {
  paymentMethods: {
    id: string
    type: string
    last4: string
    expMonth: number
    expYear: number
  }[]
  onAddPaymentMethod: () => void
  onRemovePaymentMethod: (id: string) => void
  onSetDefaultPaymentMethod: (id: string) => void
  onEditPaymentMethod: (id: string) => void
}

/**
 *
 */
export function PaymentMethod(props: PaymentMethodProps) {
  const {
    paymentMethods,
    onAddPaymentMethod,
    onRemovePaymentMethod,
    onSetDefaultPaymentMethod,
    onEditPaymentMethod,
  } = props
  const { getText } = text.useText()
  return (
    <>
      <div>
        {paymentMethods.map(paymentMethod => (
          <div key={paymentMethod.id} className="flex w-full flex-col py-2">
            <ariaComponents.Text>
              {getText('billingPagePaymentMethod', paymentMethod.type, paymentMethod.last4)}
            </ariaComponents.Text>
            <div>
              {getText(
                'billingPageExpires',
                paymentMethod.expMonth.toString(),
                paymentMethod.expYear.toString()
              )}
            </div>
          </div>
        ))}
      </div>
      <ariaComponents.Button
        variant="icon"
        size="custom"
        iconPosition="end"
        className="mt-4 text-sm"
        icon={Edit}
      >
        {getText('billingPageaddPaymentMethod')}
      </ariaComponents.Button>
    </>
  )
}
