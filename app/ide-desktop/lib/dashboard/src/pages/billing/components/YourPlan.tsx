import * as React from 'react'

import Open from 'enso-assets/open.svg'

import * as appUtils from '#/appUtils'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import type * as backend from '#/services/Backend'

/**
 *
 */
export interface YourPlanProps {
  readonly plan: backend.Plan
}

/**
 * YourPlan component
 */
export function YourPlan(props: YourPlanProps) {
  const { plan } = props
  const { getText } = text.useText()

  return (
    <div className="flex flex-col">
      <ariaComponents.Text variant="caption">{plan}</ariaComponents.Text>

      <ariaComponents.Button
        variant="icon"
        size="custom"
        className="mt-4 text-sm"
        icon={Open}
        iconPosition="end"
        href={appUtils.SUBSCRIBE_PATH}
      >
        {getText('change')}
      </ariaComponents.Button>
    </div>
  )
}
