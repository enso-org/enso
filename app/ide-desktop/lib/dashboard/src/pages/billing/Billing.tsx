/**
 * @file
 *
 * Billing page
 */
import * as React from 'react'

import GoBack from 'enso-assets/arrow_left.svg'

import * as appUtils from '#/appUtils'

import * as text from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as components from './components'

/**
 * Billing page
 */
export function Billing() {
  const { getText } = text.useText()

  return (
    <div className="h-full overflow-auto">
      <main className="mx-auto max-w-[1024px] px-4 py-6 pb-16">
        <div className="mb-12 py-2">
          <ariaComponents.Button
            href={appUtils.DASHBOARD_PATH}
            icon={GoBack}
            iconPosition="start"
            variant="icon"
            className="-ml-3"
            size="small"
          >
            {getText('billingPageBackButton')}
          </ariaComponents.Button>
          <ariaComponents.Text variant="h1">{getText('billingPageTitle')}</ariaComponents.Text>
        </div>

        <div className="flex flex-col gap-12">
          <section>
            <ariaComponents.Text variant="h3">Payment Method</ariaComponents.Text>

            <ariaComponents.Separator variant="primary" className="my-3" />

            <components.PaymentMethod
              paymentMethods={[
                {
                  id: '1',
                  type: 'Mastercard',
                  expMonth: 12,
                  expYear: 2023,
                  last4: '1234',
                },
              ]}
            />
          </section>

          <section>
            <ariaComponents.Text variant="h3">Your plan</ariaComponents.Text>

            <ariaComponents.Separator variant="primary" className="mb-3 mt-3" />

            <components.YourPlan plan="enterprise" />
          </section>

          <section>
            <ariaComponents.Text variant="h3" id="BillingInvoices">
              Invoices
            </ariaComponents.Text>

            <ariaComponents.Separator variant="primary" className="mb-3 mt-3" />

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
      </main>
    </div>
  )
}
