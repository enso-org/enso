/**
 * @file
 *
 * The billing settings tab.
 */
import * as React from 'react'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as components from './components'

/**
 * The billing settings tab.
 */
export default function BillingSettingsTab() {
  const { getText } = text.useText()

  return (
    <div className="flex w-full flex-col gap-8 md:grid md:grid-cols-2">
      <section>
        <ariaComponents.Text.Heading level="2" className="mb-2.5">
          {getText('billingSummary')}
        </ariaComponents.Text.Heading>

        <components.BillingSummary
          plan="enterprise"
          nextPayment={100}
          currency="USD"
          nextInvoiceDate="2021-09-01"
        />
      </section>

      <section>
        <ariaComponents.Text.Heading level="2" className="mb-2.5">
          {getText('paymentMethod')}
        </ariaComponents.Text.Heading>

        <components.PaymentMethod
          currentPaymentMethod={{
            id: '1',
            type: 'Mastercard',
            expMonth: 12,
            expYear: 2023,
            last4: '1234',
          }}
          paymentMethods={[
            {
              id: '1',
              type: 'Mastercard',
              expMonth: 12,
              expYear: 2023,
              last4: '1234',
            },
            {
              id: '2',
              type: 'Visa',
              expMonth: 12,
              expYear: 2023,
              last4: '5678',
            },
          ]}
        />
      </section>

      <section className="col-span-full">
        <ariaComponents.Text.Heading level="2" className="mb-2.5">
          {getText('yourPlan')}
        </ariaComponents.Text.Heading>

        <components.PlanDetails plan="team" nextPlan="enterprise" />
      </section>

      <section className="col-span-full">
        <ariaComponents.Text.Heading level="2" id="BillingInvoices" className="mb-2.5">
          {getText('invoiceHistory')}
        </ariaComponents.Text.Heading>

        <components.InvoicesTable
          titleId="BillingInvoices"
          items={[
            {
              id: '1',
              date: '2021-08-01',
              amount: 100,
              status: 'Paid',
              description: 'This is a description',
            },
            {
              id: '2',
              date: '2021-07-01',
              amount: 100,
              status: 'Paid',
              description: 'This is a description',
            },
            {
              id: '3',
              date: '2021-06-01',
              amount: 100,
              description: 'This is a description',
              status: 'Paid',
            },
          ]}
        />
      </section>
    </div>
  )
}
