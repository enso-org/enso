/**
 * @file A page to show when a user successfully subscribes to a plan.
 */
import * as router from 'react-router'
import * as routerDom from 'react-router-dom'

import * as appUtils from '#/appUtils'

import * as navigation from '#/hooks/navigateHooks'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

import * as backend from '#/services/Backend'

import * as constants from './const'

/**
 * A page to show when a user successfully subscribes to a plan.
 */
export function SubscribeSuccess() {
  const { getText } = textProvider.useText()

  const [searchParams] = routerDom.useSearchParams()
  const navigate = navigation.useNavigate()

  const plan = searchParams.get('plan') ?? backend.Plan.solo

  if (checkPlan(plan)) {
    return (
      <result.Result
        className="h-full"
        title={getText('subscribeSuccessTitle')}
        subtitle={getText(
          'subscribeSuccessSubtitle',
          `${getText(constants.PLAN_TO_TEXT_ID[plan])}`
        )}
        status="success"
      >
        <ariaComponents.Button
          variant="submit"
          size="large"
          onPress={() => {
            navigate(appUtils.DASHBOARD_PATH)
          }}
        >
          {getText('subscribeSuccessSubmit')}
        </ariaComponents.Button>
      </result.Result>
    )
  } else {
    return <router.Navigate to={appUtils.DASHBOARD_PATH} replace />
  }
}

/**
 * Check if a string is a valid plan.
 */
function checkPlan(plan: string): plan is backend.Plan {
  return plan in backend.Plan
}
