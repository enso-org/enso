/** @file Plan selector component. */
import { DIALOG_BACKGROUND } from '#/components/Dialog/variants'
import { usePaywall } from '#/hooks/billing'
import { Plan } from '#/services/Backend'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { Card } from './components'

/** Props for {@link PlanSelector} */
export interface PlanSelectorProps extends VariantProps<typeof PLAN_SELECTOR_STYLES> {
  readonly userPlan: Plan
  readonly showFreePlan: boolean
  readonly isOrganizationAdmin: boolean
  readonly plan?: Plan | null | undefined
}

const PLAN_SELECTOR_STYLES = tv({
  base: DIALOG_BACKGROUND({
    className: 'w-full snap-x overflow-auto rounded-4xl scroll-hidden',
  }),
  variants: {
    showFreePlan: { true: { grid: '2xl:grid-cols-5' } },
  },
  slots: {
    grid: 'inline-grid min-w-full gap-6 p-6 grid-cols-1fr justify-center md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4',
    card: 'min-w-64 snap-center',
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

  const classes = variants({ showFreePlan })

  return (
    <div className={classes.base()}>
      <div className={classes.grid()}>
        {(
          [
            /* eslint-disable @typescript-eslint/no-magic-numbers */
            { plan: Plan.free, period: 12 },
            { plan: Plan.solo, period: 1 },
            { plan: Plan.solo, period: 12 },
            { plan: Plan.team, period: 12 },
            { plan: Plan.enterprise, period: 12 },
            /* eslint-enable @typescript-eslint/no-magic-numbers */
          ] as const
        ).map(({ plan: newPlan, period }) => {
          if (!showFreePlan && newPlan === Plan.free) {
            return
          }

          return (
            <Card
              key={`${newPlan}/${period}`}
              plan={newPlan}
              period={period}
              modalOpen={newPlan === plan}
              userHasSubscription={userPlan !== Plan.free}
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
