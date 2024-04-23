/** @file A page in which the currently active payment plan can be changed. */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import * as stripe from '@stripe/stripe-js/pure'
import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import * as backendModule from '#/services/Backend'

import * as load from '#/utilities/load'

import * as components from './components'
import * as componentForPlan from './getComponentForPlan'

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
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const plan = searchParams.get('plan')

  const { data: stripeInstance } = reactQuery.useSuspenseQuery({
    queryKey: ['stripe', process.env.ENSO_CLOUD_STRIPE_KEY],
    staleTime: Infinity,
    queryFn: async () => {
      const stripeKey = process.env.ENSO_CLOUD_STRIPE_KEY

      if (stripeKey == null) {
        throw new Error('Stripe key not found')
      } else {
        return load
          .loadScript('https://js.stripe.com/v3/')
          .then(script =>
            stripe.loadStripe(stripeKey).finally(() => {
              script.remove()
            })
          )
          .then(maybeStripeInstance => {
            if (maybeStripeInstance == null) {
              throw new Error('Stripe instance not found')
            } else {
              return maybeStripeInstance
            }
          })
      }
    },
  })

  const onCompleteMutation = reactQuery.useMutation({
    mutationFn: async (userSelectedPlan: backendModule.Plan) => {
      const { id } = await backend.createCheckoutSession(userSelectedPlan)
      return backend.getCheckoutSession(id)
    },
    onSuccess: (data, userSelectedPlan) => {
      if (data.status === 'complete') {
        navigate({ pathname: appUtils.SUBSCRIBE_SUCCESS_PATH, search: userSelectedPlan })
      }
    },
    onError: error => {
      toastAndLog('asyncHookError', error)
    },
  })

  return (
    <div className="flex h-full w-full flex-col overflow-y-auto bg-hover-bg text-xs text-primary">
      <stripeReact.Elements stripe={stripeInstance}>
        <stripeReact.ElementsConsumer>
          {({ elements }) => {
            if (elements == null) {
              return null
            } else {
              return (
                <div className="mx-auto mt-16 flex w-full min-w-96 max-w-[1400px] flex-col items-center justify-center p-12">
                  <aria.Heading level={1} className="mb-5 self-start text-start text-4xl">
                    {getText('subscribeTitle')}
                  </aria.Heading>

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
                                onSubmit={async () => {
                                  await onCompleteMutation.mutateAsync(newPlan)
                                }}
                                elements={elements}
                                stripe={stripeInstance}
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
              )
            }
          }}
        </stripeReact.ElementsConsumer>
      </stripeReact.Elements>
    </div>
  )
}
