/** @file Plan selector component. */
import { useMutation, useQueryClient } from '@tanstack/react-query'

import { DIALOG_BACKGROUND } from '#/components/AriaComponents'
import { usePaywall } from '#/hooks/billing'
import { useAuth } from '#/providers/AuthProvider'
import { useRemoteBackendStrict } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { Plan, PLANS } from '#/services/Backend'
import { Card } from './components'
import { getComponentPerPlan } from './getComponentForPlan'

const USER_REFETCH_DELAY_MS = 3_000
const USER_REFETCH_TIMEOUT_MS = 30_000

/**
 * The mutation data for the `onCompleteMutation` mutation.
 */
interface CreateCheckoutSessionMutation {
  readonly plan: Plan
  readonly paymentMethodId: string
  readonly seats: number
  readonly period: number
}

/**
 * Props for {@link PlanSelector}
 */
export interface PlanSelectorProps {
  readonly showFreePlan?: boolean
  readonly hasTrial?: boolean
  readonly userPlan?: Plan | undefined
  readonly isOrganizationAdmin?: boolean
  readonly plan?: Plan | null | undefined
  readonly onSubscribeSuccess?: (plan: Plan, paymentMethodId: string) => void
  readonly onSubscribeError?: (error: Error) => void
}

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
  } = props

  const { getText } = useText()
  const backend = useRemoteBackendStrict()
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

  return (
    <div
      className={DIALOG_BACKGROUND({
        className: 'w-full snap-x overflow-auto rounded-4xl scroll-hidden',
      })}
    >
      <div className="inline-flex min-w-full gap-6 p-6">
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
                className="min-w-72 flex-1 snap-center"
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
                          // Invalidate all queries as the user has changed the plan.
                          await queryClient.invalidateQueries({ queryKey: ['usersMe'] })
                          break
                        } else {
                          const timePassedMs = Number(new Date()) - startEpochMs
                          if (timePassedMs > USER_REFETCH_TIMEOUT_MS) {
                            // eslint-disable-next-line no-restricted-syntax
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
