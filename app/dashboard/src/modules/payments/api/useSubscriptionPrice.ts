/**
 * @file
 *
 * This file contains the `useSubscriptionPrice` hook that is used to fetch the subscription price based on the provided parameters.
 */
import { queryOptions, useQuery } from '@tanstack/react-query'

import type { Plan } from '#/services/Backend'

import { PRICE_PER_PLAN } from '../constants'

/**
 *
 */
export interface SubscriptionPriceQueryParams {
  readonly plan: Plan
  readonly seats: number
  readonly period: number
}

/**
 * Creates a query to fetch the subscription price based on the provided parameters.
 */
export function createSubscriptionPriceQuery(params: SubscriptionPriceQueryParams) {
  return queryOptions({
    queryKey: ['getPrice', params] as const,
    queryFn: ({ queryKey }) => {
      const [, { seats, period, plan }] = queryKey

      const price = PRICE_PER_PLAN[plan]

      return Promise.resolve({
        monthly: price * seats,
        billingPeriod: period,
        total: price * seats * period,
      })
    },
  })
}

/**
 * Fetches the subscription price based on the provided parameters.
 */
export function useSubscriptionPrice(params: SubscriptionPriceQueryParams) {
  return useQuery(createSubscriptionPriceQuery(params))
}
