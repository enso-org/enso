/** @file Return the subscription price. */
import { queryOptions } from '@tanstack/react-query'
import type { Plan } from 'enso-common/src/services/Backend'
import { PRICE_BY_PLAN } from './constants'

/** Options for {@link createSubscriptionPriceQuery}. */
export interface SubscriptionPriceQueryOptions {
  readonly plan: Plan
  readonly seats: number
  readonly period: number
}

/** Create a query to fetch the subscription price. */
export function createSubscriptionPriceQuery(options: SubscriptionPriceQueryOptions) {
  return queryOptions({
    queryKey: ['getPrice', options] as const,
    queryFn: ({ queryKey }) => {
      const [, { seats, period, plan }] = queryKey

      const price = PRICE_BY_PLAN[plan]

      return Promise.resolve({
        monthlyPrice: price * seats,
        billingPeriod: period,
        totalPrice: price * seats * period,
      })
    },
  })
}
