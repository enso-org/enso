/** @file A page to show when a user successfully subscribes to a plan. */
import * as router from 'react-router'
import * as routerDom from 'react-router-dom'

import * as appUtils from '#/appUtils'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

import { PLAN_TO_TEXT_ID } from '#/modules/payments'
import { Plan, isPlan } from '#/services/Backend'

// ========================
// === SubscribeSuccess ===
// ========================

/** A page to show when a user successfully subscribes to a plan. */
export function SubscribeSuccess() {
  const { getText } = textProvider.useText()
  const [searchParams] = routerDom.useSearchParams()
  const navigate = routerDom.useNavigate()
  const plan = searchParams.get('plan') ?? Plan.solo

  if (!isPlan(plan)) {
    return <router.Navigate to={appUtils.DASHBOARD_PATH} replace />
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
              navigate(appUtils.DASHBOARD_PATH)
            }}
          >
            {getText('subscribeSuccessSubmit')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </result.Result>
    )
  }
}
