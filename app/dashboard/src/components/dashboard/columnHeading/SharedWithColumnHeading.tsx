/** @file A heading for the "Shared with" column. */
import * as React from 'react'

import PeopleIcon from '#/assets/people.svg'

import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import * as paywall from '#/components/Paywall'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  const { user } = authProvider.useFullUserSession()

  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })

  const isUnderPaywall = isFeatureUnderPaywall('share')

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <ariaComponents.Button
        variant="icon"
        size="medium"
        icon={PeopleIcon}
        aria-label={getText('sharedWithColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.sharedWith)
        }}
      />

      <div className="flex items-center gap-1">
        <aria.Text className="text-header">{getText('sharedWithColumnName')}</aria.Text>

        {isUnderPaywall && (
          <paywall.PaywallDialogButton
            feature="share"
            variant="icon"
            children={false}
            size="medium"
          />
        )}
      </div>
    </div>
  )
}
