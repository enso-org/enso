/** @file Dialog that shows the plan details, price, and the payment form. */
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Dialog } from '#/components/Dialog'
import { ErrorBoundary, ErrorDisplay } from '#/components/ErrorBoundary'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Selector } from '#/components/Inputs/Selector'
import { Separator } from '#/components/Separator'
import { Suspense } from '#/components/Suspense'
import { Text } from '#/components/Text'
import { Plan } from '#/services/Backend'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import type { GetText } from '$/providers/text'
import { useQuery } from '@tanstack/react-query'
import type { TextId } from 'enso-common/src/text'
import {
  MAX_SEATS_BY_PLAN,
  PRICE_BY_PLAN,
  PRICE_CURRENCY,
  TRIAL_DURATION_DAYS,
} from '../../../constants'
import { createSubscriptionPriceQuery } from '../../../useSubscriptionPrice'
import { PlanFeatures } from './PlanFeatures'

const PLAN_TO_SEATS_DESCRIPTION_ID = {
  free: 'freePlanSeatsDescription',
  solo: 'soloPlanSeatsDescription',
  team: 'teamPlanSeatsDescription',
  enterprise: 'enterprisePlanSeatsDescription',
} satisfies {
  [PlanType in Plan]: TextId & `${PlanType}PlanSeatsDescription`
}

/** Props for {@link PlanSelectorDialog}. */
export interface PlanSelectorDialogProps {
  readonly plan: Plan
  readonly planName: string
  readonly features: string[]
  readonly title: string
  readonly onSubmit?: ((seats: number, interval: number) => Promise<void> | void) | undefined
  /** Whether the user clicked on the trial button. */
  readonly isTrialing?: boolean
}

/** Get the string representation of a billing period. */
function billingPeriodToString(getText: GetText, item: number) {
  switch (item) {
    case 1: {
      return getText('billingPeriodOneMonth')
    }
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    case 12: {
      return getText('billingPeriodOneYear')
    }
    default: {
      return getText('unknownPlaceholder')
    }
  }
}

/** Dialog that shows the plan details, price, and the payment form. */
export function PlanSelectorDialog(props: PlanSelectorDialogProps) {
  const { title, planName, features, plan, isTrialing = false, onSubmit } = props
  const { getText, locale } = useText()

  const price = PRICE_BY_PLAN[plan]
  const maxSeats = MAX_SEATS_BY_PLAN[plan]

  const form = Form.useForm({
    mode: 'onChange',
    schema: (z) =>
      z.object({
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
    onSubmit: ({ seats, period }) => onSubmit?.(seats, period),
  })

  const seats = Form.useWatch({ name: 'seats', control: form.control })
  const period = Form.useWatch({ name: 'period', control: form.control })

  const formatter = new Intl.NumberFormat(locale, { style: 'currency', currency: PRICE_CURRENCY })

  return (
    <Dialog size="xxlarge" closeButton="floating" aria-label={title} padding="large">
      <Text.Heading level="2" variant="subtitle" weight="medium" disableLineHeightCompensation>
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

      {plan !== Plan.solo && <Separator orientation="horizontal" className="my-4" />}

      <ErrorBoundary>
        <Suspense>
          <Form form={form} className="mt-1">
            <div className="grid grid-cols-[1fr]">
              <div className="flex flex-col gap-4">
                <div>
                  {plan !== Plan.solo && (
                    <Text variant="subtitle">{getText('adjustYourPlan')}</Text>
                  )}

                  <div className="mt-1">
                    {plan === Plan.solo && (
                      <Selector
                        form={form}
                        name="period"
                        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                        items={[1, 12]}
                        label={getText('billingPeriod')}
                      >
                        {(item) => billingPeriodToString(getText, item)}
                      </Selector>
                    )}

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
                      description={getText(PLAN_TO_SEATS_DESCRIPTION_ID[plan], maxSeats)}
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
                  </div>
                </div>
              </div>

              <Summary
                plan={plan}
                seats={seats}
                period={period}
                formatter={formatter}
                isInvalid={form.formState.errors.seats != null}
              />
            </div>

            <Form.Submit>
              {isTrialing ? getText('startTrial') : getText('subscribeSubmit')}
            </Form.Submit>
          </Form>
        </Suspense>
      </ErrorBoundary>
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

  return isError ?
      <ErrorDisplay
        error={error}
        title={getText('asyncHookError')}
        resetErrorBoundary={() => refetch()}
      />
    : <div className="mt-4 flex flex-col">
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
                {billingPeriodToString(getText, period)}
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
