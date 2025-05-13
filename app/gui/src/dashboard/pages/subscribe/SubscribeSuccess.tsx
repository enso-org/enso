/** @file A page to show when a user successfully subscribes to a plan. */

import * as appUtils from '#/appUtils'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

import { PLAN_TO_TEXT_ID } from '#/modules/payments'
import { Plan, isPlan } from '#/services/Backend'
import { useRouterInReact } from '$/providers/react'

/** A page to show when a user successfully subscribes to a plan. */
export function SubscribeSuccess() {
  const { getText } = textProvider.useText()
  const { router, searchParams } = useRouterInReact()
  const plan = searchParams.get('plan') ?? Plan.solo

  if (!isPlan(plan)) {
    void router.replace(appUtils.DASHBOARD_PATH)
    return
  } else {
    return (
      <result.Result
        className="h-full"
        title={getText('subscribeSuccessTitle')}
        subtitle={getText('subscribeSuccessSubtitle', getText(PLAN_TO_TEXT_ID[plan]))}
        status="success"
      >
        <ariaComponents.ButtonGroup align="center">
          <ariaComponents.Button
            variant="submit"
            size="large"
            onPress={() => {
              void router.push(appUtils.DASHBOARD_PATH)
            }}
          >
            {getText('subscribeSuccessSubmit')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </result.Result>
    )
  }
}
