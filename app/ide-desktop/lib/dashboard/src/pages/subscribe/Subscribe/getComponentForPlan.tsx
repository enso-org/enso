/** @file Logic to get the component for a given plan. */
import * as React from 'react'

import type * as stripe from '@stripe/stripe-js'

import OpenInNewTabIcon from 'enso-assets/open.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as constants from '#/pages/subscribe/constants'
import * as components from '#/pages/subscribe/Subscribe/components'

import * as ariaComponents from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'

import * as string from '#/utilities/string'

// =========================
// === SubmitButtonProps ===
// =========================

/** The props for the submit button. */
interface SubmitButtonProps {
  readonly onSubmit: (paymentMethodId: string) => Promise<void>
  readonly elements: stripe.StripeElements
  readonly stripe: stripe.Stripe
  readonly plan: backendModule.Plan
  readonly defaultOpen?: boolean
}

// ========================
// === ComponentForPlan ===
// ========================

/** The component for a plan. */
export interface ComponentForPlan {
  readonly pricing: text.TextId
  readonly features: text.TextId
  readonly title: text.TextId
  readonly subtitle: text.TextId
  readonly learnMore: () => React.ReactNode
  readonly submitButton: (props: SubmitButtonProps) => React.ReactNode
}

// ===========================
// === getComponentForPlan ===
// ===========================

const COMPONENT_FOR_PLAN: Record<backendModule.Plan, ComponentForPlan> = {
  [backendModule.Plan.solo]: {
    learnMore: () => {
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.Button
          variant="link"
          href="https://enso.org/pricing"
          target="_blank"
          icon={OpenInNewTabIcon}
          iconPosition="end"
        >
          {getText('learnMore')}
        </ariaComponents.Button>
      )
    },
    pricing: 'soloPlanPricing',
    submitButton: props => {
      const { onSubmit, elements, stripe, defaultOpen = false, plan } = props
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.DialogTrigger defaultOpen={defaultOpen}>
          <ariaComponents.Button variant={'outline'} fullWidth size="medium" rounded="full">
            {getText('subscribe')}
          </ariaComponents.Button>

          <ariaComponents.Dialog title={getText('upgradeTo', string.capitalizeFirst(plan))}>
            <components.SubscribeForm onSubmit={onSubmit} elements={elements} stripe={stripe} />
          </ariaComponents.Dialog>
        </ariaComponents.DialogTrigger>
      )
    },
    features: 'soloPlanFeatures',
    subtitle: 'soloPlanSubtitle',
    title: constants.PLAN_TO_TEXT_ID['solo'],
  },
  [backendModule.Plan.team]: {
    learnMore: () => {
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.Button
          variant="link"
          href="https://enso.org/pricing"
          target="_blank"
          icon={OpenInNewTabIcon}
          iconPosition="end"
        >
          {getText('learnMore')}
        </ariaComponents.Button>
      )
    },
    pricing: 'teamPlanPricing',
    features: 'teamPlanFeatures',
    title: constants.PLAN_TO_TEXT_ID['team'],
    subtitle: 'teamPlanSubtitle',
    submitButton: props => {
      const { onSubmit, elements, stripe, defaultOpen = false, plan } = props
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.DialogTrigger defaultOpen={defaultOpen}>
          <ariaComponents.Button variant={'submit'} fullWidth size="medium" rounded="full">
            {getText('subscribe')}
          </ariaComponents.Button>

          <ariaComponents.Dialog title={getText('upgradeTo', string.capitalizeFirst(plan))}>
            <components.SubscribeForm onSubmit={onSubmit} elements={elements} stripe={stripe} />
          </ariaComponents.Dialog>
        </ariaComponents.DialogTrigger>
      )
    },
  },
  [backendModule.Plan.enterprise]: {
    learnMore: () => {
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.Button
          variant="link"
          href="https://enso.org/pricing"
          target="_blank"
          icon={OpenInNewTabIcon}
          iconPosition="end"
        >
          {getText('learnMore')}
        </ariaComponents.Button>
      )
    },
    pricing: 'enterprisePlanPricing',
    features: 'enterprisePlanFeatures',
    title: constants.PLAN_TO_TEXT_ID['enterprise'],
    subtitle: 'enterprisePlanSubtitle',
    submitButton: () => {
      const { getText } = textProvider.useText()
      return (
        <ariaComponents.Button
          fullWidth
          variant="primary"
          size="medium"
          rounded="full"
          target="_blank"
          href="mailto:contact@enso.org?subject=Upgrading%20to%20Organization%20Plan"
        >
          {getText('contactSales')}
        </ariaComponents.Button>
      )
    },
  },
}

/** Get the component for a given plan.
 * @throws {Error} if the plan is invalid. */
export function getComponentForPlan(plan: backendModule.Plan, getText: textProvider.GetText) {
  const result = COMPONENT_FOR_PLAN[plan]
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  if (result == null) {
    throw new Error(`Invalid plan: ${plan}`)
  } else {
    return {
      ...result,
      features: getText(result.features).split(';'),
    }
  }
}
