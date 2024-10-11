/** @file A page in which the currently active payment plan can be changed. */
import { useNavigate, useSearchParams } from 'react-router-dom'

import { DASHBOARD_PATH, SUBSCRIBE_SUCCESS_PATH } from '#/appUtils'
import Back from '#/assets/arrow_left.svg'
import { Button, Text } from '#/components/AriaComponents'
import { PlanSelector } from '#/modules/payments'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { isPlan } from '#/services/Backend'

/**
 * A page in which the currently active payment plan can be changed.
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
  const { getText } = useText()
  const navigate = useNavigate()
  const { user } = useFullUserSession()

  const [searchParams] = useSearchParams()

  const maybePlan = searchParams.get('plan')

  const chosenPlan = isPlan(maybePlan) ? maybePlan : null

  return (
    <div className="flex h-full w-full flex-col overflow-y-auto bg-hover-bg">
      <div className="mx-auto mt-16 flex w-full min-w-96 max-w-[1400px] flex-col items-start justify-center p-12">
        <div className="flex flex-col items-start">
          <Button variant="icon" size="medium" icon={Back} href={DASHBOARD_PATH} className="-ml-2">
            {getText('returnToDashboard')}
          </Button>

          <Text.Heading level={1} variant="custom" className="mb-5 self-start text-start text-4xl">
            {getText('subscribeTitle')}
          </Text.Heading>
        </div>

        <PlanSelector
          plan={chosenPlan}
          showFreePlan={false}
          userPlan={user.plan}
          isOrganizationAdmin={user.isOrganizationAdmin}
          onSubscribeSuccess={(plan) => {
            navigate({
              pathname: SUBSCRIBE_SUCCESS_PATH,
              search: new URLSearchParams({ plan }).toString(),
            })
          }}
        />
      </div>
    </div>
  )
}
