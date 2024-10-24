/** @file Plan selector component. */
import { useMutation, useQueryClient } from '@tanstack/react-query'

import { DIALOG_BACKGROUND } from '#/components/AriaComponents'
import { usePaywall } from '#/hooks/billing'
import { useAuth } from '#/providers/AuthProvider'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { Plan, PLANS } from '#/services/Backend'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { Card } from './components'
import { getComponentPerPlan } from './getComponentForPlan'

const USER_REFETCH_DELAY_MS = 3_000
const USER_REFETCH_TIMEOUT_MS = 30_000

/** The mutation data for the `onCompleteMutation` mutation. */
interface CreateCheckoutSessionMutation {
  readonly plan: Plan
  readonly paymentMethodId: string
  readonly seats: number
  readonly period: number
}

/** Props for {@link PlanSelector} */
export interface PlanSelectorProps extends VariantProps<typeof PLAN_SELECTOR_STYLES> {
  readonly showFreePlan?: boolean
  readonly hasTrial?: boolean
  readonly userPlan?: Plan | undefined
  readonly isOrganizationAdmin?: boolean
  readonly plan?: Plan | null | undefined
  readonly onSubscribeSuccess?: (plan: Plan, paymentMethodId: string) => void
  readonly onSubscribeError?: (error: Error) => void
}

const PLAN_SELECTOR_STYLES = tv({
  base: DIALOG_BACKGROUND({
    className: 'w-full snap-x overflow-auto rounded-4xl scroll-hidden',
  }),
  variants: {
    showFreePlan: {
      true: {
        grid: 'grid-cols-1fr md:grid-cols-2 xl:grid-cols-4',
      },
      false: {
        grid: 'grid-cols-1fr md:grid-cols-3 justify-center',
      },
    },
  },
  slots: {
    grid: 'inline-grid min-w-full gap-6 p-6',
    card: 'min-w-64 snap-center',
  },
})

/**
 * Plan selector component.
 * Shows the available plans and allows the user to subscribe to one.
 */
export function PlanSelector(props: PlanSelectorProps) {
  const {
    onSubscribeSuccess,
    onSubscribeError,
    plan,
    userPlan,
    showFreePlan = true,
    hasTrial = true,
    isOrganizationAdmin = false,
    variants = PLAN_SELECTOR_STYLES,
  } = props

  const { getText } = useText()
  const backend = useRemoteBackend()
  const { refetchSession } = useAuth()
  const { getPaywallLevel } = usePaywall({ plan: userPlan })

  const queryClient = useQueryClient()
  const onCompleteMutation = useMutation({
    mutationFn: async (mutationData: CreateCheckoutSessionMutation) => {
      const { id } = await backend.createCheckoutSession({
        plan: mutationData.plan,
        paymentMethodId: mutationData.paymentMethodId,
        quantity: mutationData.seats,
        interval: mutationData.period,
      })

      return backend.getCheckoutSession(id).then((data) => {
        if (['trialing', 'active'].includes(data.status)) {
          return data
        } else {
          throw new Error('The payment was not successful. Please try again or contact support.')
        }
      })
    },
    onError: (error) => onSubscribeError?.(error),
  })

  const classes = variants({ showFreePlan })

  return (
    <div className={classes.base()}>
      <div className={classes.grid()}>
        {PLANS.map((newPlan) => {
          const paywallLevel = getPaywallLevel(newPlan)
          const userPaywallLevel = getPaywallLevel(userPlan)
          const planProps = getComponentPerPlan(newPlan, getText)

          if (showFreePlan || newPlan !== Plan.free) {
            const isCurrentPlan =
              newPlan === userPlan || (newPlan === Plan.free && userPlan === undefined)

            return (
              <Card
                key={newPlan}
                className={classes.card()}
                features={planProps.features}
                subtitle={planProps.subtitle}
                title={planProps.title}
                elevated={planProps.elevated === true ? 'xxlarge' : 'none'}
                submitButton={
                  <planProps.submitButton
                    onSubmit={async (paymentMethodId, seats, period) => {
                      await onCompleteMutation.mutateAsync({
                        plan: newPlan,
                        paymentMethodId,
                        seats,
                        period,
                      })

                      const startEpochMs = Number(new Date())

                      while (true) {
                        const { data: session } = await refetchSession()
                        if (session && 'user' in session && session.user.plan === newPlan) {
                          onSubscribeSuccess?.(newPlan, paymentMethodId)
                          // Invalidate "users me" query as the user has changed the plan.
                          await queryClient.invalidateQueries({
                            queryKey: [backend.type, 'usersMe'],
                          })
                          break
                        } else {
                          const timePassedMs = Number(new Date()) - startEpochMs
                          if (timePassedMs > USER_REFETCH_TIMEOUT_MS) {
                            throw new Error(
                              'Timed out waiting for subscription, please contact support to continue.',
                            )
                          } else {
                            await new Promise((resolve) => {
                              window.setTimeout(resolve, USER_REFETCH_DELAY_MS)
                            })
                          }
                        }
                      }
                    }}
                    plan={newPlan}
                    userHasSubscription={userPlan != null && userPlan !== Plan.free}
                    isCurrent={isCurrentPlan}
                    isDowngrade={userPaywallLevel > paywallLevel}
                    defaultOpen={newPlan === plan}
                    features={planProps.features}
                    canTrial={hasTrial}
                    planName={getText(newPlan)}
                    isOrganizationAdmin={isOrganizationAdmin}
                  />
                }
                learnMore={<planProps.learnMore />}
                pricing={planProps.pricing}
              />
            )
          } else {
            return null
          }
        })}
      </div>
    </div>
  )
}
