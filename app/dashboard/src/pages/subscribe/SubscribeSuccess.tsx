/** @file A page to show when a user successfully subscribes to a plan. */
import { useSearchParams } from 'react-router-dom'

import { Button, ButtonGroup } from '#/components/AriaComponents'
import Navigate from '#/components/Navigate'
import { Result } from '#/components/Result'
import { useNavigate } from '#/hooks/routerHooks'
import { PLAN_TO_TEXT_ID } from '#/modules/payments'
import { useText } from '#/providers/TextProvider'
import { Plan, isPlan } from '#/services/Backend'

// ========================
// === SubscribeSuccess ===
// ========================

/** A page to show when a user successfully subscribes to a plan. */
export function SubscribeSuccess() {
  const { getText } = useText()
  const [searchParams] = useSearchParams()
  const navigate = useNavigate()
  const plan = searchParams.get('plan') ?? Plan.solo

  if (!isPlan(plan)) {
    return <Navigate to="/drive" replace />
  } else {
    return (
      <Result
        className="h-full"
        title={getText('subscribeSuccessTitle')}
        subtitle={getText('subscribeSuccessSubtitle', getText(PLAN_TO_TEXT_ID[plan]))}
        status="success"
      >
        <ButtonGroup align="center">
          <Button
            variant="submit"
            size="large"
            onPress={() => {
              navigate('/drive')
            }}
          >
            {getText('subscribeSuccessSubmit')}
          </Button>
        </ButtonGroup>
      </Result>
    )
  }
}
