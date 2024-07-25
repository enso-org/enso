/** @file A page to show when a user successfully subscribes to a plan. */
import * as routerDom from 'react-router-dom'

import { useNavigate } from '#/hooks/routerHooks'

import * as textProvider from '#/providers/TextProvider'

import * as constants from '#/pages/subscribe/constants'

import * as ariaComponents from '#/components/AriaComponents'
import Navigate from '#/components/Navigate'
import * as result from '#/components/Result'

import * as backend from '#/services/Backend'

// ========================
// === SubscribeSuccess ===
// ========================

/** A page to show when a user successfully subscribes to a plan. */
export function SubscribeSuccess() {
  const { getText } = textProvider.useText()
  const [searchParams] = routerDom.useSearchParams()
  const navigate = useNavigate()
  const plan = searchParams.get('plan') ?? backend.Plan.solo

  if (!backend.isPlan(plan)) {
    return <Navigate to="/drive" replace />
  } else {
    return (
      <result.Result
        className="h-full"
        title={getText('subscribeSuccessTitle')}
        subtitle={getText('subscribeSuccessSubtitle', getText(constants.PLAN_TO_TEXT_ID[plan]))}
        status="success"
      >
        <ariaComponents.ButtonGroup align="center">
          <ariaComponents.Button
            variant="submit"
            size="large"
            onPress={() => {
              navigate('/drive')
            }}
          >
            {getText('subscribeSuccessSubmit')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </result.Result>
    )
  }
}
