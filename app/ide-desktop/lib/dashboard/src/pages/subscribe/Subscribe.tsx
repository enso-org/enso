/** @file A page in which the currently active payment plan can be changed. */
import * as React from 'react'

import * as stripeReact from '@stripe/react-stripe-js'
import type * as stripeTypes from '@stripe/stripe-js'
import * as stripe from '@stripe/stripe-js/pure'
import * as toast from 'react-toastify'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import Modal from '#/components/Modal'

import * as backendModule from '#/services/Backend'

import * as config from '#/utilities/config'
import * as load from '#/utilities/load'
import * as string from '#/utilities/string'

// =================
// === Constants ===
// =================

let stripePromise: Promise<stripeTypes.Stripe | null> | null = null

/** The delay in milliseconds before redirecting back to the main page. */
const REDIRECT_DELAY_MS = 1_500

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
  const stripeKey = config.ACTIVE_CONFIG.stripeKey
  const navigate = navigateHooks.useNavigate()
  // Plan that the user has currently selected, if any (e.g., 'solo', 'team', etc.).
  const [plan, setPlan] = React.useState(() => {
    const initialPlan = new URLSearchParams(location.search).get('plan')
    return backendModule.isPlan(initialPlan) ? initialPlan : backendModule.Plan.solo
  })
  // A client secret used to access details about a Checkout Session on the Stripe API. A Checkout
  // Session represents a customer's session as they are in the process of paying for a
  // subscription. The client secret is provided by Stripe when the Checkout Session is created.
  const [clientSecret, setClientSecret] = React.useState('')
  // The ID of a Checkout Session on the Stripe API. This is the same as the client secret, minus
  // the secret part. Without the secret part, the session ID can be safely stored in the URL
  // query.
  const [sessionId, setSessionId] = React.useState<backendModule.CheckoutSessionId | null>(null)
  // The status of a Checkout Session on the Stripe API. This stores whether or not the Checkout
  // Session is complete (i.e., the user has provided payment information), and if so, whether
  // payment has been confirmed.
  const [sessionStatus, setSessionStatus] =
    React.useState<backendModule.CheckoutSessionStatus | null>(null)
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  if (stripePromise == null) {
    stripePromise = load.loadScript('https://js.stripe.com/v3/').then(async script => {
      const innerStripe = await stripe.loadStripe(stripeKey)
      script.remove()
      return innerStripe
    })
  }

  React.useEffect(() => {
    void (async () => {
      try {
        const checkoutSession = await backend.createCheckoutSession(plan)
        setClientSecret(checkoutSession.clientSecret)
        setSessionId(checkoutSession.id)
      } catch (error) {
        toastAndLog(null, error)
      }
    })()
  }, [backend, plan, /* should never change */ toastAndLog])

  React.useEffect(() => {
    if (sessionStatus?.status === 'complete') {
      toast.toast.success('Your plan has successfully been upgraded!')
      window.setTimeout(() => {
        navigate(appUtils.DASHBOARD_PATH)
      }, REDIRECT_DELAY_MS)
    }
  }, [sessionStatus?.status, navigate])

  const onComplete = React.useCallback(() => {
    if (sessionId != null) {
      void (async () => {
        try {
          setSessionStatus(await backend.getCheckoutSession(sessionId))
        } catch (error) {
          toastAndLog(null, error)
        }
      })()
    }
    // Stripe does not allow this callback to change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sessionId])

  return (
    <Modal centered className="bg-black/10 text-primary text-xs">
      <div
        data-testid="subscribe-modal"
        className="flex flex-col gap-2 bg-frame-selected backdrop-blur-3xl rounded-2xl p-8 w-full max-w-md max-h-[100vh]"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        <div className="self-center text-xl">Upgrade to {string.capitalizeFirst(plan)}</div>
        <div className="flex items-stretch rounded-full bg-gray-500/30 text-base h-8">
          {backendModule.PLANS.map(newPlan => (
            <button
              key={newPlan}
              disabled={plan === newPlan}
              type="button"
              className="flex-1 grow rounded-full disabled:bg-frame"
              onClick={event => {
                event.stopPropagation()
                setPlan(newPlan)
              }}
            >
              {string.capitalizeFirst(newPlan)}
            </button>
          ))}
        </div>
        {sessionId && clientSecret ? (
          <div className="overflow-auto">
            <stripeReact.EmbeddedCheckoutProvider
              key={sessionId}
              stripe={stripePromise}
              // Above, `sessionId` is updated when the `checkoutSession` is created.
              // This triggers a fetch of the session's `status`.
              // The `status` is not going to be `complete` at that point
              // (unless the user completes the checkout process before the fetch is complete).
              // So the `status` needs to be fetched again when the `checkoutSession` is updated.
              // This is done by passing a function to `onComplete`.
              options={{ clientSecret, onComplete }}
            >
              <stripeReact.EmbeddedCheckout />
            </stripeReact.EmbeddedCheckoutProvider>
          </div>
        ) : (
          <div className="h-155 transition-all" />
        )}
      </div>
    </Modal>
  )
}
