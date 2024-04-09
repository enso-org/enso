/** @file A page in which the currently active payment plan can be changed. */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import * as stripe from '@stripe/stripe-js/pure'
import * as reactQuery from '@tanstack/react-query'
import * as toast from 'react-toastify'

import * as load from 'enso-common/src/load'

import * as appUtils from '#/appUtils'
import type * as text from '#/text'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as searchParamsHooks from '#/hooks/searchParamsStateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import Modal from '#/components/Modal'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'

import * as string from '#/utilities/string'

// =================
// === Constants ===
// =================

/** The delay in milliseconds before redirecting back to the main page. */
const REDIRECT_DELAY_MS = 1_500

const PLAN_TO_TEXT_ID: Readonly<Record<backendModule.Plan, text.TextId>> = {
  [backendModule.Plan.solo]: 'soloPlanName',
  [backendModule.Plan.team]: 'teamPlanName',
  [backendModule.Plan.enterprise]: 'enterprisePlanName',
} satisfies { [Plan in backendModule.Plan]: `${Plan}PlanName` }

// =================
// === Subscribe ===
// =================

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
 * paymentStatus: 'no_payment_required' || 'paid' || 'unpaid' }`). */
export default function Subscribe() {
  const { getText } = textProvider.useText()
  const navigate = navigateHooks.useNavigate()

  const [plan, setPlan] = searchParamsHooks.useSearchParamsState(
    'plan',
    backendModule.Plan.solo,
    (raw): raw is backendModule.Plan => backendModule.isPlan(raw)
  )

  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const [{ data: checkoutSession }, { data: stripeInstance }] = reactQuery.useSuspenseQueries({
    queries: [
      {
        queryKey: ['checkoutSession', plan],
        queryFn: async () => {
          // backend.createCheckoutSession(plan)
          return {
            clientSecret: 'cs_foo',
            id: 'cs_bar',
          } as backendModule.CheckoutSession
        },
      },
      {
        queryKey: ['stripe', process.env.ENSO_CLOUD_STRIPE_KEY],
        staleTime: Infinity,
        queryFn: async () => {
          const stripeKey = process.env.ENSO_CLOUD_STRIPE_KEY

          if (stripeKey == null) {
            throw new Error('Stripe key not found')
          } else {
            return load.loadScript('https://js.stripe.com/v3/').then(script =>
              stripe.loadStripe(stripeKey).finally(() => {
                script.remove()
              })
            )
          }
        },
      },
    ],
  })

  const onCompleteMutation = reactQuery.useMutation({
    mutationKey: ['checkoutSessionStatus'],
    mutationFn: async () => backend.getCheckoutSession(checkoutSession.id),
    onSuccess: data => {
      if (data.status === 'complete') {
        toast.toast.success('Your plan has successfully been upgraded!')
        window.setTimeout(() => {
          navigate(appUtils.DASHBOARD_PATH)
        }, REDIRECT_DELAY_MS)
      }
    },
    onError: error => {
      toastAndLog('asyncHookError', error)
    },
  })

  const { clientSecret, id: sessionId } = checkoutSession

  return (
    <Modal centered className="bg-hover-bg text-xs text-primary">
      <div
        data-testid="subscribe-modal"
        className="flex max-h-screen w-full max-w-md flex-col gap-modal rounded-default bg-selected-frame p-auth backdrop-blur-default"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        <div className="self-center text-xl">
          {getText('upgradeTo', string.capitalizeFirst(plan))}
        </div>
        <div className="flex h-row items-stretch rounded-full bg-gray-500/30 text-base">
          {backendModule.PLANS.map(newPlan => (
            <UnstyledButton
              key={newPlan}
              isDisabled={plan === newPlan}
              className="flex-1 grow rounded-full disabled:bg-frame"
              onPress={() => {
                setPlan(newPlan)
              }}
            >
              {getText(PLAN_TO_TEXT_ID[newPlan])}
            </UnstyledButton>
          ))}
        </div>
        <div className="overflow-auto">
          <stripeReact.EmbeddedCheckoutProvider
            key={sessionId}
            stripe={stripeInstance}
            // Above, `sessionId` is updated when the `checkoutSession` is created.
            // This triggers a fetch of the session's `status`.
            // The `status` is not going to be `complete` at that point
            // (unless the user completes the checkout process before the fetch is complete).
            // So the `status` needs to be fetched again when the `checkoutSession` is updated.
            // This is done by passing a function to `onComplete`.
            options={{ clientSecret, onComplete: onCompleteMutation.mutate }}
          >
            <stripeReact.EmbeddedCheckout />
          </stripeReact.EmbeddedCheckoutProvider>
        </div>
      </div>
    </Modal>
  )
}
