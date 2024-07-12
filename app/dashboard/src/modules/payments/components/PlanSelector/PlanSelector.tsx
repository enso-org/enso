/**
 * @file
 *
 * Plan selector component.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import { usePaywall } from '#/hooks/billing'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import { DIALOG_BACKGROUND } from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'

import * as components from './components'
import * as componentForPlan from './getComponentForPlan'

/**
 * The mutation data for the `onCompleteMutation` mutation.
 */
interface CreateCheckoutSessionMutation {
  readonly plan: backendModule.Plan
  readonly paymentMethodId: string
  readonly seats: number
}

/**
 * Props for {@link PlanSelector}
 */
export interface PlanSelectorProps {
  readonly showFreePlan?: boolean
  readonly hasTrial?: boolean
  readonly userPlan?: backendModule.Plan | undefined
  readonly plan?: backendModule.Plan | null | undefined
  readonly onSubscribeSuccess?: (plan: backendModule.Plan, paymentMethodId: string) => void
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
  } = props

  const { getText } = textProvider.useText()
  const backend = backendProvider.useRemoteBackendStrict()
  const { getPaywallLevel } = usePaywall({ plan: userPlan })

  const onCompleteMutation = reactQuery.useMutation({
    mutationFn: async (mutationData: CreateCheckoutSessionMutation) => {
      const { id } = await backend.createCheckoutSession({
        plan: mutationData.plan,
        paymentMethodId: mutationData.paymentMethodId,
        quantity: mutationData.seats,
      })

      return backend.getCheckoutSession(id).then(data => {
        if (['trialing', 'active'].includes(data.status)) {
          return data
        } else {
          throw new Error(
            'Session not complete, please contact the support team or try with another payment method.'
          )
        }
      })
    },
    onSuccess: (_, mutationData) =>
      onSubscribeSuccess?.(mutationData.plan, mutationData.paymentMethodId),
    onError: error => onSubscribeError?.(error),
    meta: { invalidates: [['userMe'], [['organization']]], awaitInvalidates: true },
  })

  return (
    <div
      className={DIALOG_BACKGROUND({
        className: 'w-full snap-x overflow-auto rounded-4xl scroll-hidden',
      })}
    >
      <div className="inline-flex min-w-full gap-6 p-6">
        {backendModule.PLANS.map(newPlan => {
          const paywallLevel = getPaywallLevel(newPlan)
          const userPaywallLevel = getPaywallLevel(userPlan)
          const planProps = componentForPlan.getComponentPerPlan(newPlan, getText)

          if (showFreePlan || newPlan !== backendModule.Plan.free) {
            const isCurrentPlan =
              newPlan === userPlan ||
              (newPlan === backendModule.Plan.free && userPlan === undefined)

            return (
              <components.Card
                key={newPlan}
                className="min-w-72 flex-1 snap-center"
                features={planProps.features}
                subtitle={planProps.subtitle}
                title={planProps.title}
                elevated={planProps.elevated === true ? 'xxlarge' : 'none'}
                submitButton={
                  <planProps.submitButton
                    onSubmit={async (paymentMethodId, seats) => {
                      await onCompleteMutation.mutateAsync({
                        plan: newPlan,
                        paymentMethodId,
                        seats,
                      })
                    }}
                    plan={newPlan}
                    userHasSubscription={userPlan != null && userPlan !== backendModule.Plan.free}
                    isCurrent={isCurrentPlan}
                    isDowngrade={userPaywallLevel > paywallLevel}
                    defaultOpen={newPlan === plan}
                    features={planProps.features}
                    canTrial={hasTrial}
                    planName={getText(newPlan)}
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
