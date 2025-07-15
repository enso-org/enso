/** @file A card representing a plan. */
import Check from '#/assets/check_mark.svg'
import OpenInNewTabIcon from '#/assets/open.svg'
import { Button } from '#/components/Button'
import { Separator } from '#/components/Separator'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import type { PaywallLevel } from '#/hooks/billing'
import type { SubscribeButtonProps } from '#/modules/payments/components/PlanSelector/components/SubscribeButton'
import { SubscribeButton } from '#/modules/payments/components/PlanSelector/components/SubscribeButton'
import { PLAN_TO_TEXT_ID, PRICE_BY_PLAN } from '#/modules/payments/constants'
import type { PlanBillingPeriod } from '#/services/Backend'
import { Plan } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends } from '$/providers/backends'
import { useText } from '$/providers/react'
import * as analytics from '$/utils/analytics'
import type { TextId } from 'enso-common/src/text'
import * as React from 'react'

/** The mutation data for the `createCheckoutSession` mutation. */
interface CreateCheckoutSessionMutationParams {
  readonly plan: Plan
  readonly seats: number
  readonly period: PlanBillingPeriod
}

const TEXT_ID_FOR_BILLING_PERIOD: Record<PlanBillingPeriod, TextId & `billingPeriod${string}`> = {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  1: 'billingPeriodMonthly',
  // eslint-disable-next-line @typescript-eslint/naming-convention, @typescript-eslint/no-magic-numbers
  12: 'billingPeriodAnnually',
}

/** The component for a plan. */
export interface PropsForPlan<PlanVariant extends Plan> {
  readonly pricing: TextId & `${PlanVariant}PlanPricing`
  readonly features: TextId & `${PlanVariant}PlanFeatures`
  readonly title: TextId & `${PlanVariant}PlanName`
  readonly subtitle: TextId & `${PlanVariant}PlanSubtitle`
  readonly submitButton: (props: SubscribeButtonProps) => React.ReactNode
  readonly elevated?: boolean
}

const PROPS_FOR_PLAN: {
  readonly [PlanVariant in Plan]: PropsForPlan<PlanVariant>
} = {
  free: {
    pricing: 'freePlanPricing',
    features: 'freePlanFeatures',
    title: PLAN_TO_TEXT_ID['free'],
    subtitle: 'freePlanSubtitle',
    submitButton: (props) => <SubscribeButton {...props} isDisabled={true} />,
  },
  [Plan.solo]: {
    pricing: 'soloPlanPricing',
    features: 'soloPlanFeatures',
    subtitle: 'soloPlanSubtitle',
    title: PLAN_TO_TEXT_ID['solo'],
    submitButton: SubscribeButton,
  },
  [Plan.team]: {
    pricing: 'teamPlanPricing',
    features: 'teamPlanFeatures',
    title: PLAN_TO_TEXT_ID['team'],
    subtitle: 'teamPlanSubtitle',
    elevated: true,
    submitButton: SubscribeButton,
  },
  [Plan.enterprise]: {
    pricing: 'enterprisePlanPricing',
    features: 'enterprisePlanFeatures',
    title: PLAN_TO_TEXT_ID['enterprise'],
    subtitle: 'enterprisePlanSubtitle',
    submitButton: () => {
      // False positive
      // eslint-disable-next-line react-hooks/rules-of-hooks
      const { getText } = useText()

      return (
        <Button fullWidth isDisabled variant="outline" size="medium" rounded="full">
          {getText('comingSoon')}
        </Button>
      )
    },
  },
}

const CARD_STYLES = tv({
  base: 'flex flex-col border-0.5',
  variants: {
    elevated: {
      none: '',
      true: 'shadow-primary/15 shadow',
      small: 'shadow-primary/15 shadow-sm',
      medium: 'shadow-primary/15 shadow-md',
      large: 'shadow-primary/15 shadow-lg',
      xlarge: 'shadow-primary/15 shadow-xl',
      xxlarge: 'shadow-primary/15 shadow-2xl',
      xxxlarge: 'shadow-primary/15 shadow-3xl',
    },
    highlighted: {
      true: 'outline outline-1.5 -outline-offset-1 outline-primary',
      false: 'border-primary/30',
    },
    rounded: {
      none: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
      xxxxlarge: 'rounded-4xl',
    },
    size: {
      medium: { base: 'p-[19.5px]', separator: '-mx-[19.5px]' },
    },
  },
  slots: {
    features: '',
    separator: 'w-auto',
  },
  defaultVariants: {
    elevated: 'none',
    rounded: 'xxxxlarge',
    size: 'medium',
  },
})

/** Props for {@link Card}. */
export interface CardProps {
  readonly plan: Plan
  readonly period: PlanBillingPeriod
  readonly modalOpen: boolean
  readonly userHasSubscription: boolean
  readonly isOrganizationAdmin: boolean
  readonly isCurrent: boolean
  readonly paywallLevel: PaywallLevel
  readonly userPaywallLevel: PaywallLevel
  readonly className?: string | undefined
}

/** Card component */
export function Card(props: CardProps) {
  const {
    plan,
    period,
    modalOpen,
    userHasSubscription,
    isOrganizationAdmin,
    isCurrent,
    paywallLevel,
    userPaywallLevel,
    className,
  } = props

  const { getText } = useText()
  const { remoteBackend } = useBackends()

  const propsForPlan = PROPS_FOR_PLAN[plan]
  const { title, subtitle, pricing } = propsForPlan
  const features = getText(propsForPlan.features).split(';')
  const elevated = propsForPlan.elevated === true ? 'xxlarge' : 'none'

  const styles = CARD_STYLES({ elevated })

  const onSubmit = useMutationCallback({
    mutationFn: async (mutationData: CreateCheckoutSessionMutationParams) => {
      const planInfo = {
        price: mutationData.plan,
        quantity: mutationData.seats,
        interval: mutationData.period,
      }
      analytics.checkout.before(planInfo)
      const { url } = await remoteBackend.createCheckoutSession(planInfo)
      window.open(url, '_blank')?.focus()
    },
  })

  const titleTextBase = getText(title)
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const shouldShowAnnualVariant = plan === Plan.solo && period === 12
  const titleText =
    shouldShowAnnualVariant ? getText('annualPlanVariant', titleTextBase) : titleTextBase

  return (
    <div className={styles.base({ className })}>
      <Text.Heading level={2} disableLineHeightCompensation>
        {titleText}
      </Text.Heading>

      <Text elementType="p" variant="subtitle" weight="medium" disableLineHeightCompensation>
        {getText(subtitle)}
      </Text>

      <Text variant="body" weight="bold" disableLineHeightCompensation>
        {getText(pricing, PRICE_BY_PLAN[plan], getText(TEXT_ID_FOR_BILLING_PERIOD[period]))}
      </Text>

      <div className="my-4">
        <propsForPlan.submitButton
          onSubmit={(seats) => onSubmit({ plan, seats, period })}
          plan={plan}
          period={period}
          userHasSubscription={userHasSubscription}
          isCurrent={isCurrent}
          isDowngrade={userPaywallLevel > paywallLevel}
          defaultOpen={modalOpen}
          features={features}
          planName={getText(plan)}
          isOrganizationAdmin={isOrganizationAdmin}
        />
      </div>

      <Separator variant="primary" className={styles.separator()} orientation="horizontal" />

      {features.length > 0 && (
        <div className="mt-4">
          <ul className="flex flex-col gap-2">
            {features.map((feature, index) => (
              <li key={index} className="flex items-center gap-1">
                <span className="-mb-[1px] flex h-4 w-4 flex-none place-items-center rounded-full bg-green/30">
                  <SvgMask src={Check} className="text-green" />
                </span>

                <Text variant="body" weight="medium" disableLineHeightCompensation>
                  {feature}
                </Text>
              </li>
            ))}
          </ul>
        </div>
      )}

      {plan !== Plan.free && (
        <div className="mt-4">
          <Button
            variant="link"
            href="https://ensoanalytics.com/pricing"
            target="_blank"
            icon={OpenInNewTabIcon}
            iconPosition="end"
            size="medium"
          >
            {getText('learnMore')}
          </Button>
        </div>
      )}
    </div>
  )
}
