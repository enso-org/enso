/**
 * @file
 *
 * Dialog that shows the plan details, price, and the payment form.
 */
import * as React from 'react'

import type { PaymentMethod } from '@stripe/stripe-js'
import { useQuery } from '@tanstack/react-query'

import { type GetText, useText } from '#/providers/TextProvider'

import {
  Button,
  Checkbox,
  Dialog,
  Form,
  Input,
  Selector,
  Separator,
  Text,
} from '#/components/AriaComponents'
import { ErrorDisplay } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'

import type { Plan } from '#/services/Backend'

import { twMerge } from '#/utilities/tailwindMerge'

import { createSubscriptionPriceQuery, useCreatePaymentMethodMutation } from '../../../api'
import {
  MAX_SEATS_BY_PLAN,
  PRICE_BY_PLAN,
  PRICE_CURRENCY,
  TRIAL_DURATION_DAYS,
} from '../../../constants'
import { AddPaymentMethodForm, createAddPaymentMethodFormSchema } from '../../AddPaymentMethodForm'
import { StripeProvider } from '../../StripeProvider'
import { PlanFeatures } from './PlanFeatures'

/** Props for {@link PlanSelectorDialog}. */
export interface PlanSelectorDialogProps {
  readonly plan: Plan
  readonly planName: string
  readonly features: string[]
  readonly title: string
  readonly onSubmit?:
    | ((
        paymentMethodId: PaymentMethod['id'],
        seats: number,
        interval: number,
      ) => Promise<void> | void)
    | undefined
  /** Whether the user clicked on the trial button. */
  readonly isTrialing?: boolean
}

/** Get the string representation of a billing period. */
function billingPeriodToString(getText: GetText, item: number) {
  return (
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    item === 12 ? getText('billingPeriodOneYear') : getText('billingPeriodThreeYears')
  )
}

/** Dialog that shows the plan details, price, and the payment form. */
export function PlanSelectorDialog(props: PlanSelectorDialogProps) {
  const { title, planName, features, plan, isTrialing = false, onSubmit } = props
  const { getText, locale } = useText()

  const price = PRICE_BY_PLAN[plan]
  const maxSeats = MAX_SEATS_BY_PLAN[plan]

  const createPaymentMethodMutation = useCreatePaymentMethodMutation()

  const form = Form.useForm({
    mode: 'onChange',
    schema: (z) =>
      createAddPaymentMethodFormSchema(z, getText).extend({
        seats: z
          .number()
          .int()
          .positive()
          .min(1)
          .max(maxSeats, { message: getText('wantMoreSeats') }),
        period: z.number(),
        agree: z
          .array(z.string())
          .min(1, { message: getText('licenseAgreementCheckboxError') })
          .max(1, { message: getText('licenseAgreementCheckboxError') }),
      }),
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    defaultValues: { seats: 1, period: 12, agree: [] },
    onSubmit: async ({ cardElement, stripeInstance, seats, period }) => {
      const res = await createPaymentMethodMutation.mutateAsync({
        cardElement,
        stripeInstance,
      })

      return onSubmit?.(res.paymentMethod.id, seats, period)
    },
  })

  const seats = Form.useWatch({ name: 'seats', control: form.control })
  const period = Form.useWatch({ name: 'period', control: form.control })

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

              <Form form={form} className="mt-1">
                <Selector
                  form={form}
                  name="period"
                  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                  items={[12, 36]}
                  label={getText('billingPeriod')}
                >
                  {(item) => billingPeriodToString(getText, item)}
                </Selector>

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

                <Checkbox.Group
                  form={form}
                  name="agree"
                  description={
                    <>
                      {getText('slsaLicenseAgreementDescription1')}{' '}
                      <Button
                        variant="link"
                        href="https://www.ensoanalytics.com/SLSA"
                        target="_blank"
                      >
                        {getText('SLSA')}
                      </Button>
                      {getText('slsaLicenseAgreementDescription2')}
                    </>
                  }
                >
                  <Checkbox value="agree">{getText('licenseAgreementCheckbox')}</Checkbox>
                </Checkbox.Group>
              </Form>
            </div>
          </div>

          <div>
            <div className="my-4">
              <Summary
                plan={plan}
                seats={seats}
                period={period}
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
                    onSubmit={(paymentMethodId) => onSubmit?.(paymentMethodId, seats, period)}
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

/** Props for {@link Summary}. */
interface SummaryProps {
  readonly plan: Plan
  readonly seats: number
  readonly period: number
  readonly formatter: Intl.NumberFormat
  readonly isInvalid?: boolean
  readonly isTrialing?: boolean
}

/** Displays a summary of the plan details and the total price. */
function Summary(props: SummaryProps) {
  const { plan, seats, period, formatter, isInvalid = false } = props
  const { getText } = useText()

  const { data, isLoading, isError, refetch, error } = useQuery({
    ...createSubscriptionPriceQuery({ plan, seats, period }),
    enabled: !isInvalid,
  })

  const billingPeriodText = billingPeriodToString(getText, period)

  return isError ?
      <ErrorDisplay
        error={error}
        title={getText('asyncHookError')}
        resetErrorBoundary={() => refetch()}
      />
    : <div className="flex flex-col">
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
                {formatter.format(data.monthlyPrice)}
              </Text>
            )}
          </div>

          <div className="table-row">
            <Text className="table-cell w-[0%]" variant="body" nowrap>
              {getText('billingPeriod')}
            </Text>

            {data && (
              <Text className="table-cell" variant="body">
                {billingPeriodText}
              </Text>
            )}
          </div>

          <div className="table-row">
            <Text className="table-cell w-[0%]" variant="body" nowrap>
              {getText('originalPrice')}
            </Text>
            {data && (
              <Text className="table-cell" variant="body">
                {formatter.format(data.fullPrice)}
              </Text>
            )}
          </div>

          <div className="table-row">
            <Text className="table-cell w-[0%]" variant="body" nowrap>
              {getText('youSave')}
            </Text>
            {data && (
              <Text
                className="table-cell"
                color={data.discount > 0 ? 'success' : 'primary'}
                variant="body"
              >
                {formatter.format(data.discount)}
              </Text>
            )}
          </div>

          <div className="table-row">
            <Text className="table-cell w-[0%]" variant="body" nowrap>
              {getText('subtotalPrice')}
            </Text>
            {data && (
              <Text className="table-cell" variant="body">
                {formatter.format(data.totalPrice)}
              </Text>
            )}
          </div>
        </div>
      </div>
}
