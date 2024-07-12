/** @file A page in which the currently active payment plan can be changed. */
import * as React from 'react'

import * as router from 'react-router-dom'

import Back from '#/assets/arrow_left.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'
import * as paymentModule from '#/modules/payments'

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
  const { getText } = textProvider.useText()
  const navigate = router.useNavigate()
  const { user } = authProvider.useFullUserSession()

  const [searchParams] = router.useSearchParams()

  const maybePlan = searchParams.get('plan')

  const chosenPlan = backendModule.isPlan(maybePlan) ? maybePlan : null

  return (
    <div className="flex h-full w-full flex-col overflow-y-auto bg-hover-bg">
      <div className="mx-auto mt-16 flex w-full min-w-96 max-w-[1400px] flex-col items-start justify-center p-12">
        <div className="flex flex-col items-start">
          <ariaComponents.Button
            variant="icon"
            size="medium"
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

        <paymentModule.PlanSelector
          plan={chosenPlan}
          showFreePlan={false}
          userPlan={user.plan}
          onSubscribeSuccess={plan => {
            navigate({ pathname: appUtils.SUBSCRIBE_SUCCESS_PATH, search: `plan=${plan}` })
          }}
        />
      </div>
    </div>
  )
}
