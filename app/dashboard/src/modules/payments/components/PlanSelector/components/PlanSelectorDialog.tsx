/**
 * @file
 *
 * Dialog that shows the plan details, price, and the payment form.
 */
import * as React from 'react'

import type { PaymentMethod } from '@stripe/stripe-js'
import { useQuery } from '@tanstack/react-query'

import { useText } from '#/providers/TextProvider'

import { Dialog, Form, Input, Separator, Text } from '#/components/AriaComponents'
import { ErrorDisplay } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'

import { createSubscriptionPriceQuery } from '#/modules/payments'
import { Plan } from '#/services/Backend'

import { twMerge } from '#/utilities/tailwindMerge'

import { PRICE_CURRENCY, PRICE_PER_PLAN, TRIAL_DURATION_DAYS } from '../../../constants'
import { ADD_PAYMENT_METHOD_FORM_SCHEMA, AddPaymentMethodForm } from '../../AddPaymentMethodForm'
import { StripeProvider } from '../../StripeProvider'
import { PlanFeatures } from './PlanFeatures'

/**
 * Props for {@link PlanSelectorDialog}.
 */
export interface PlanSelectorDialogProps {
  readonly plan: Plan
  readonly planName: string
  readonly features: string[]
  readonly title: string
  readonly onSubmit?:
    | ((paymentMethodId: PaymentMethod['id'], seats: number) => Promise<void> | void)
    | undefined
  /**
   * Whether the user clicked on the trial button.
   */
  readonly isTrialing?: boolean
}

/**
 * Dialog that shows the plan details, price, and the payment form.
 */
export function PlanSelectorDialog(props: PlanSelectorDialogProps) {
  const { title, planName, features, plan, isTrialing = false, onSubmit } = props
  const { getText, locale } = useText()

  const price = PRICE_PER_PLAN[plan]
  const maxSeats =
    plan === Plan.enterprise ? Infinity
    : plan === Plan.team ? 10
    : 1

  const form = Form.useForm({
    schema: (z) =>
      ADD_PAYMENT_METHOD_FORM_SCHEMA.extend({
        seats: z
          .number()
          .int()
          .positive()
          .min(1)
          .max(maxSeats, { message: getText('wantMoreSeats') }),
      }),
    defaultValues: { seats: 1 },
    mode: 'onChange',
  })

  const seats = form.watch('seats')

  const formatter = React.useMemo(
    () => new Intl.NumberFormat(locale, { style: 'currency', currency: PRICE_CURRENCY }),
    [locale],
  )

  return (
    <Dialog size="xxxlarge" closeButton="floating" aria-label={title}>
      <div className="mx-auto max-w-screen-sm pb-4">
        <Text.Heading
          level="2"
          variant="subtitle"
          weight="medium"
          disableLineHeightCompensation
          className="-mt-0.5"
        >
          {title}
        </Text.Heading>

        <Text variant="h1" weight="medium" disableLineHeightCompensation className="mb-2 block">
          {isTrialing ?
            getText('tryFree', TRIAL_DURATION_DAYS) +
            getText('priceTemplate', formatter.format(price), getText('billedAnnually'))
          : getText('priceTemplate', formatter.format(price), getText('billedAnnually'))}
        </Text>

        <div>
          <Text.Heading level="3" variant="body" weight="semibold" className="mb-1">
            {getText('upgradeCTA', planName)}
          </Text.Heading>

          <PlanFeatures features={features} />
        </div>

        <Separator orientation="horizontal" className="my-4" />

        <div className="grid grid-cols-[1fr]">
          <div className="flex flex-col gap-4">
            <div>
              <Text variant="subtitle">{getText('adjustYourPlan')}</Text>

              <Form form={form}>
                <Input
                  isRequired
                  readOnly={maxSeats === 1}
                  form={form}
                  name="seats"
                  type="number"
                  inputMode="decimal"
                  size="small"
                  min="1"
                  label={getText('seats')}
                  description={getText(`${plan}PlanSeatsDescription`, maxSeats)}
                />
              </Form>
            </div>
          </div>

          <div>
            <div className="my-4">
              <Summary
                plan={plan}
                seats={seats}
                formatter={formatter}
                isInvalid={form.formState.errors.seats != null}
              />
            </div>

            <Suspense>
              <StripeProvider>
                {({ stripe, elements }) => (
                  <AddPaymentMethodForm
                    form={form}
                    elements={elements}
                    stripeInstance={stripe}
                    submitText={isTrialing ? getText('startTrial') : getText('subscribeSubmit')}
                    onSubmit={(paymentMethodId) => onSubmit?.(paymentMethodId, seats)}
                  />
                )}
              </StripeProvider>
            </Suspense>
          </div>
        </div>
      </div>
    </Dialog>
  )
}

/**
 * Props for {@link Summary}.
 */
interface SummaryProps {
  readonly plan: Plan
  readonly seats: number
  readonly formatter: Intl.NumberFormat
  readonly isInvalid?: boolean
  readonly isTrialing?: boolean
}

/**
 * Displays a summary of the plan details and the total price.
 */
function Summary(props: SummaryProps) {
  const { plan, seats, formatter, isInvalid = false } = props
  const { getText } = useText()

  const { data, isLoading, isError, refetch, error } = useQuery({
    ...createSubscriptionPriceQuery({
      plan,
      seats,
      period: 12,
    }),
    enabled: !isInvalid,
  })

  if (isError) {
    // eslint-disable-next-line no-restricted-syntax
    return (
      <ErrorDisplay
        error={error}
        title={getText('asyncHookError')}
        resetErrorBoundary={() => refetch()}
      />
    )
  }

  return (
    <div className="flex flex-col">
      <Text variant="subtitle">{getText('summary')}</Text>

      <div
        className={twMerge(
          '-ml-4 table table-auto border-spacing-x-4 transition-[filter] duration-200',
          (isLoading || isInvalid) && 'pointer-events-none blur-[4px]',
          isLoading && 'animate-pulse duration-1000',
        )}
      >
        <div className="table-row">
          <Text className="table-cell w-[0%]" variant="body" nowrap>
            {getText('priceMonthly')}
          </Text>
          {data && (
            <Text className="table-cell " variant="body">
              {formatter.format(data.monthly)}
            </Text>
          )}
        </div>

        <div className="table-row">
          <Text className="table-cell w-[0%]" variant="body" nowrap>
            {getText('billingPeriod')}
          </Text>

          {data && (
            <Text className="table-cell" variant="body">
              {getText('months', data.billingPeriod)}
            </Text>
          )}
        </div>

        <div className="table-row">
          <Text className="table-cell w-[0%]" variant="body" nowrap>
            {getText('totalPrice')}
          </Text>
          {data && (
            <Text className="table-cell" variant="body">
              {formatter.format(data.total)}
            </Text>
          )}
        </div>
      </div>
    </div>
  )
}
