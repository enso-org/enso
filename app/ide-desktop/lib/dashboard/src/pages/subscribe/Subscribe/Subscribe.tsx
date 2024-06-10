/** @file A page in which the currently active payment plan can be changed. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'

import Back from 'enso-assets/arrow_left.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'

import * as components from './components'
import * as componentForPlan from './getComponentForPlan'

/**
 * The mutation data for the `onCompleteMutation` mutation.
 */
interface CreateCheckoutSessionMutation {
  readonly plan: backendModule.Plan
  readonly paymentMethodId: string
}

/** A page in which the currently active payment plan can be changed.
 *
 * This page can be in one of several states:
 *
 * 1. Initial (i.e. `plan = null, clientSecret = '', sessionStatus = null`),
 * 2. Plan selected (e.g. `plan = 'solo', clientSecret = '', sessionStatus = null`),
 * 3. Session created (e.g. `plan = 'solo', clientSecret = 'cs_foo',
 * sessionStatus.status = { status: 'open' || 'complete' || 'expired',
 * paymentStatus: 'no_payment_required' || 'paid' || 'unpaid' }`),
 * 4. Session complete (e.g. `plan = 'solo', clientSecret = 'cs_foo',
 * sessionStatus.status = { status: 'complete',
 * paymentStatus: 'no_payment_required' || 'paid' || 'unpaid' }`).
 */
export function Subscribe() {
  const navigate = navigateHooks.useNavigate()
  const { getText } = textProvider.useText()

  const [searchParams] = router.useSearchParams()
  const { backend } = backendProvider.useStrictBackend()

  const plan = searchParams.get('plan')

  const onCompleteMutation = reactQuery.useMutation({
    mutationFn: async (mutationData: CreateCheckoutSessionMutation) => {
      const { id } = await backend.createCheckoutSession({
        plan: mutationData.plan,
        paymentMethodId: mutationData.paymentMethodId,
      })
      return backend.getCheckoutSession(id)
    },
    onSuccess: (data, mutationData) => {
      if (['trialing', 'active'].includes(data.status)) {
        navigate({ pathname: appUtils.SUBSCRIBE_SUCCESS_PATH, search: `plan=${mutationData.plan}` })
        return
      } else {
        throw new Error(
          'Session not complete, please contact the support team or try with another payment method.'
        )
      }
    },
  })

  return (
    <div className="flex h-full w-full flex-col overflow-y-auto bg-hover-bg text-xs text-primary">
      <div className="mx-auto mt-16 flex w-full min-w-96 max-w-[1400px] flex-col items-start justify-center p-12">
        <div className="flex flex-col items-start">
          <ariaComponents.Button
            variant="icon"
            icon={Back}
            href={appUtils.DASHBOARD_PATH}
            className="-ml-2"
          >
            {getText('returnToDashboard')}
          </ariaComponents.Button>

          <ariaComponents.Text.Heading
            level={1}
            variant="custom"
            className="mb-5 self-start text-start text-4xl"
          >
            {getText('subscribeTitle')}
          </ariaComponents.Text.Heading>
        </div>

        <div className="w-full rounded-default bg-selected-frame p-8">
          <div className="flex gap-6 overflow-auto scroll-hidden">
            {backendModule.PLANS.map(newPlan => {
              const planProps = componentForPlan.getComponentPerPlan(newPlan, getText)

              return (
                <components.Card
                  key={newPlan}
                  className="min-w-64 flex-1"
                  features={planProps.features}
                  subtitle={planProps.subtitle}
                  title={planProps.title}
                  submitButton={
                    <planProps.submitButton
                      onSubmit={async paymentMethodId => {
                        await onCompleteMutation.mutateAsync({
                          plan: newPlan,
                          paymentMethodId,
                        })
                      }}
                      plan={newPlan}
                      defaultOpen={newPlan === plan}
                    />
                  }
                  learnMore={<planProps.learnMore />}
                  pricing={planProps.pricing}
                />
              )
            })}
          </div>
        </div>
      </div>
    </div>
  )
}
