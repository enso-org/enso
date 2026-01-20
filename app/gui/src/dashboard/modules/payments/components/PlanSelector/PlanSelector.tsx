/** @file Plan selector component. */
import { DIALOG_BACKGROUND } from '#/components/Dialog/variants'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { useBackends } from '$/providers/backends'
import { useSuspenseQuery } from '@tanstack/react-query'
import * as backend from 'enso-common/src/services/Backend'
import { Card } from './components'

/** Props for {@link PlanSelector} */
export interface PlanSelectorProps extends VariantProps<typeof PLAN_SELECTOR_STYLES> {
  readonly userPlan: backend.Plan
  readonly showFreePlan: boolean
  readonly isOrganizationAdmin: boolean
  readonly plan?: backend.Plan | null | undefined
}

const PLAN_SELECTOR_STYLES = tv({
  base: DIALOG_BACKGROUND({
    className: 'w-full snap-x overflow-auto rounded-4xl scroll-hidden',
  }),
  variants: {
    showFreePlan: { true: { grid: '2xl:auto-cols-5' } },
  },
  slots: {
    grid: 'inline-grid min-w-full gap-6 p-6 grid-flow-col auto-cols-1fr justify-center md:auto-cols-2 lg:auto-cols-3 xl:auto-cols-4',
    card: 'min-w-72 snap-center',
  },
})

/**
 * Plan selector component.
 * Shows the available plans and allows the user to subscribe to one.
 */
export function PlanSelector(props: PlanSelectorProps) {
  const {
    plan,
    userPlan,
    showFreePlan,
    isOrganizationAdmin,
    variants = PLAN_SELECTOR_STYLES,
  } = props

  const { getPaywallLevel } = usePaywall({ plan: userPlan })
  const { remoteBackend } = useBackends()

  const { data: config } = useSuspenseQuery(
    backendQueryOptions(remoteBackend, 'getPaymentsConfig', []),
  )
  const classes = variants({ showFreePlan })

  return (
    <div className={classes.base()}>
      <div className={classes.grid()}>
        {config.cards.map(({ plan: newPlan, period, title, subtitle, pricing, features }) => {
          return (
            <Card
              key={`${newPlan}/${period}`}
              plan={newPlan}
              period={period}
              texts={{ title, subtitle, pricing, features }}
              modalOpen={newPlan === plan}
              userHasSubscription={userPlan !== backend.Plan.free}
              isOrganizationAdmin={isOrganizationAdmin}
              isCurrent={newPlan === userPlan}
              paywallLevel={getPaywallLevel(newPlan)}
              userPaywallLevel={getPaywallLevel(userPlan)}
              className={classes.card()}
            />
          )
        })}
      </div>
    </div>
  )
}
