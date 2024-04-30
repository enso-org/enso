/**
 * @file
 *
 * Plan details.
 * Shows summary with plan name, price, and features.
 * Also provides a button to upgrade the plan and show features you will get.
 */
import * as React from 'react'

import NotAvailableIcon from 'enso-assets/cross.svg'
import CheckIcon from 'enso-assets/tick.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

import type * as types from './types'

/**
 * Props for {@link PlanDetails}.
 */
export interface PlanDetailsProps {
  readonly plan: types.PlanType
  readonly nextPlan?: types.PlanType | null
}

/**
 * Plan details.
 * Shows summary with plan name, price, and features.
 * Also provides a button to upgrade the plan and show features you will get.
 */
export function PlanDetails(props: PlanDetailsProps) {
  const { plan, nextPlan } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Alert variant="outline" size="custom" rounded="xlarge" border="small">
      <div className="flex items-center gap-2 border-b border-primary/30 px-3 py-1.5">
        <ariaComponents.Text variant="subtitle" className="flex flex-col">
          {getText(plan)}
          <ariaComponents.Text weight="normal">
            {getText(`${plan}PlanSubtitle`)}
          </ariaComponents.Text>
        </ariaComponents.Text>

        <ariaComponents.Button
          variant="outline"
          href="/subscribe"
          size="small"
          rounded="full"
          className="ml-auto"
        >
          {getText('comparePlans')}
        </ariaComponents.Button>
      </div>

      <div className="flex p-3">
        <article className="flex flex-1 flex-col" role="list">
          {getText(FEATURES_BY_PLAN[plan])
            .split(';')
            .map(feature => (
              <div
                key={feature}
                className="flex items-center gap-1.5"
                role="listitem"
                aria-label={feature}
              >
                <div className="flex aspect-square w-4 place-items-center rounded-full bg-green/30 text-green">
                  <SvgMask src={CheckIcon} className="h-full w-full" />
                </div>

                <ariaComponents.Text>{feature}</ariaComponents.Text>
              </div>
            ))}
        </article>

        {nextPlan && (
          <>
            <ariaComponents.Separator orientation="vertical" className="mx-4" />
            <article className="flex flex-1 flex-col items-start">
              <ariaComponents.Text
                elementType="h3"
                variant="body"
                weight="bold"
                className="-mb-0.5 -mt-1"
              >
                {getText('featuresYouWillGet', getText(nextPlan))}
              </ariaComponents.Text>

              <div className="flex flex-col" role="list">
                {getText(FEATURES_BY_PLAN[nextPlan])
                  .split(';')
                  .map(feature => (
                    <div
                      key={feature}
                      className="flex items-center gap-1.5"
                      role="listitem"
                      aria-label={feature}
                    >
                      <div className="flex aspect-square w-4 place-items-center rounded-full bg-primary/60 text-white/80">
                        <SvgMask src={NotAvailableIcon} className="h-full w-full rotate-45" />
                      </div>

                      <ariaComponents.Text>{feature}</ariaComponents.Text>
                    </div>
                  ))}
              </div>

              <ariaComponents.Button
                variant="link"
                rounded="full"
                size="small"
                href="/subscribe"
                className="mt-1"
              >
                {getText('upgradeTo', getText(nextPlan))}
              </ariaComponents.Button>
            </article>
          </>
        )}
      </div>
    </ariaComponents.Alert>
  )
}

const FEATURES_BY_PLAN: Record<(typeof types.PLANS)[number], text.TextId> = {
  solo: 'soloPlanFeatures',
  team: 'teamPlanFeatures',
  enterprise: 'enterprisePlanFeatures',
}
