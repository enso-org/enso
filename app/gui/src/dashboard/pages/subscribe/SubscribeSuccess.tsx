/** @file A page to show when a user successfully subscribes to a plan. */
import { Button } from '#/components/Button'
import { Result } from '#/components/Result'
import { PLAN_TO_TEXT_ID } from '#/modules/payments'
import { Plan, isPlan } from '#/services/Backend'
import { DASHBOARD_PATH } from '$/appUtils'
import { useRouter, useText } from '$/providers/react'

/** A page to show when a user successfully subscribes to a plan. */
export function SubscribeSuccess() {
  const { getText } = useText()
  const { router, searchParams } = useRouter()
  const plan = searchParams.get('plan') ?? Plan.solo

  if (!isPlan(plan)) {
    void router.replace(DASHBOARD_PATH)
    return
  } else {
    return (
      <Result
        className="h-full"
        title={getText('subscribeSuccessTitle')}
        subtitle={getText('subscribeSuccessSubtitle', getText(PLAN_TO_TEXT_ID[plan]))}
        status="success"
      >
        <Button.Group align="center">
          <Button
            variant="submit"
            size="large"
            onPress={() => {
              void router.push(DASHBOARD_PATH)
            }}
          >
            {getText('subscribeSuccessSubmit')}
          </Button>
        </Button.Group>
      </Result>
    )
  }
}
