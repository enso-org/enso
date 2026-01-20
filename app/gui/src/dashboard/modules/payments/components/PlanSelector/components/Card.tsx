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
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import * as appUtils from '$/appUtils'
import { getContactPage } from '$/appUtils'
import { useBackends } from '$/providers/backends'
import { useRouter, useText } from '$/providers/react'
import * as analytics from '$/utils/analytics'
import { Plan, type PlanBillingPeriod } from 'enso-common/src/services/Backend'
import * as React from 'react'

/** The mutation data for the `createCheckoutSession` mutation. */
interface CreateCheckoutSessionMutationParams {
  readonly plan: Plan
  readonly seats: number
  readonly period: PlanBillingPeriod
}

/** The component for a plan. */
export interface PropsForPlan {
  readonly submitButton: (props: SubscribeButtonProps) => React.ReactNode
  readonly elevated?: boolean
}

const PROPS_FOR_PLAN: { readonly [PlanVariant in Plan]: PropsForPlan } = {
  free: {
    submitButton: (props) => <SubscribeButton {...props} isDisabled={true} />,
  },
  [Plan.solo]: {
    submitButton: SubscribeButton,
  },
  [Plan.team]: {
    elevated: true,
    submitButton: SubscribeButton,
  },
  [Plan.enterprise]: {
    submitButton: () => {
      // False positive
      // eslint-disable-next-line react-hooks/rules-of-hooks
      const { getText } = useText()

      return (
        <Button fullWidth variant="outline" size="medium" rounded="full" href={getContactPage()}>
          {getText('contactUs')}
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

/** Props for {@link Card}s texts. */
export interface Texts {
  readonly title: string
  readonly subtitle: string
  readonly pricing: string
  readonly features: readonly string[]
}

/** Props for {@link Card}. */
export interface CardProps {
  readonly plan: Plan
  readonly period: PlanBillingPeriod
  readonly texts: Texts
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
    texts,
    modalOpen,
    userHasSubscription,
    isOrganizationAdmin,
    isCurrent,
    paywallLevel,
    userPaywallLevel,
    className,
  } = props

  const { title, subtitle, pricing, features } = texts

  const { getText } = useText()
  const { remoteBackend } = useBackends()
  const { router } = useRouter()

  const propsForPlan = PROPS_FOR_PLAN[plan]
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
      await router.push(`${appUtils.PAYMENTS_SUCCESS_PATH}`)
    },
  })

  return (
    <div className={styles.base({ className })}>
      <Text.Heading level={2} disableLineHeightCompensation>
        {title}
      </Text.Heading>

      <Text elementType="p" variant="subtitle" weight="medium" disableLineHeightCompensation>
        {subtitle}
      </Text>

      <Text variant="body" weight="bold" disableLineHeightCompensation>
        {pricing}
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
            href={`${$config.ENSO_HOST}/pricing`}
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
