/**
 * @file A page to show when a user successfully subscribes to a plan.
 */
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

/**
 * A page to show when a user successfully subscribes to a plan.
 */
export function SubscribeSuccess() {
  const { getText } = textProvider.useText()

  return (
    <result.Result
      className="h-full"
      title="subscribeSuccessTitle"
      subtitle="subscribeSuccessSubtitle"
      status="success"
    >
      <ariaComponents.Button variant="submit">
        {getText('subscribeSuccessSubmit')}
      </ariaComponents.Button>
    </result.Result>
  )
}
