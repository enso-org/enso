import * as React from 'react'

import * as twMerge from 'tailwind-merge'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import type * as dateTime from '#/utilities/dateTime'

import type * as types from './types'

/**
 *
 */
export interface BillingSummaryProps {
  readonly className?: string
  readonly plan: types.PlanType
  readonly nextPayment: number
  readonly nextInvoiceDate: dateTime.Rfc3339DateTime
  readonly currency: string
}

/**
 *
 */
export function BillingSummary(props: BillingSummaryProps) {
  const { plan, className = '', nextPayment, currency = 'USD', nextInvoiceDate } = props

  const { getText, locale } = textProvider.useText()

  return (
    <div
      className={twMerge.twMerge(
        '-ml-5 -mt-1 table table-auto border-spacing-x-5 border-spacing-y-1',
        className
      )}
    >
      <div className="table-row">
        <ariaComponents.Text weight="medium" className="table-cell">
          {getText('currentPlan')}
        </ariaComponents.Text>
        <ariaComponents.Text weight="bold" className="table-cell">
          {getText(plan)}
        </ariaComponents.Text>
      </div>
      <div className="table-row">
        <ariaComponents.Text weight="medium" className="table-cell">
          {getText('nextPayment')}
        </ariaComponents.Text>
        <ariaComponents.Text weight="bold" className="table-cell">
          {nextPayment.toLocaleString(locale, {
            style: 'currency',
            currency: currency,
          })}
        </ariaComponents.Text>
      </div>
      <div className="table-row">
        <ariaComponents.Text weight="medium" className="table-cell">
          {getText('nextInvoiceDate')}
        </ariaComponents.Text>
        <ariaComponents.Text
          weight="bold"
          className="table-cell"
          tooltip={new Date(nextInvoiceDate).toLocaleString(locale)}
          tooltipDisplay="always"
        >
          {new Date(nextInvoiceDate).toLocaleString(locale, {
            dateStyle: 'long',
          })}
        </ariaComponents.Text>
      </div>
    </div>
  )
}
