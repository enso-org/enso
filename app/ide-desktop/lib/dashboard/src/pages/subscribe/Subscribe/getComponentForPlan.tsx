/**
 * @file
 *
 * This file contains the logic to get the component for a given plan.
 */
import * as React from 'react'

import OpenInNewTabIcon from 'enso-assets/open.svg'

import * as appUtils from '#/appUtils'
import type * as text from 'enso-common/src/text'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import AddPaymentMethodModal from '#/modals/AddPaymentMethodModal'

import * as backendModule from '#/services/Backend'

import * as constants from '../constants'

/**
 * The props for the submit button.
 */
interface SubmitButtonProps {
  readonly onSubmit: (paymentMethodId: string) => Promise<void>
  readonly plan: backendModule.Plan
  readonly defaultOpen?: boolean
}

/**
 * The component for a plan.
 */
export interface ComponentForPlan {
  readonly pricing: text.TextId
  readonly features: text.TextId
  readonly title: text.TextId
  readonly subtitle: text.TextId
  readonly learnMore: () => React.ReactNode
  readonly submitButton: (props: SubmitButtonProps) => React.ReactNode
}

/**
 * Get the component for a given plan.
 * @throws Error if the plan is invalid.
 */
export function getComponentPerPlan(plan: backendModule.Plan, getText: textProvider.GetText) {
  // we double check that the plan is valid
  // eslint-disable-next-line no-restricted-syntax
  const result = COMPONENT_PER_PLAN[plan]

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

const COMPONENT_PER_PLAN: Record<backendModule.Plan, ComponentForPlan> = {
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
          size="medium"
        >
          {getText('learnMore')}
        </ariaComponents.Button>
      )
    },
    pricing: 'soloPlanPricing',
    submitButton: props => {
      const { onSubmit, defaultOpen = false, plan } = props
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.DialogTrigger defaultOpen={defaultOpen}>
          <ariaComponents.Button variant={'outline'} fullWidth size="large" rounded="full">
            {getText('subscribe')}
          </ariaComponents.Button>

          <AddPaymentMethodModal
            title={getText('upgradeTo', getText(plan))}
            onSubmit={onSubmit}
            submitText={getText('subscribeSubmit')}
          />
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
          size="medium"
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
      const { onSubmit, defaultOpen = false, plan } = props
      const { getText } = textProvider.useText()

      return (
        <ariaComponents.DialogTrigger defaultOpen={defaultOpen}>
          <ariaComponents.Button variant={'submit'} fullWidth size="large" rounded="full">
            {getText('subscribe')}
          </ariaComponents.Button>

          <AddPaymentMethodModal
            title={getText('upgradeTo', getText(plan))}
            onSubmit={onSubmit}
            submitText={getText('subscribeSubmit')}
          />
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
          size="medium"
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
          size="large"
          rounded="full"
          target="_blank"
          href={appUtils.getContactSalesURL()}
        >
          {getText('contactSales')}
        </ariaComponents.Button>
      )
    },
  },
}
