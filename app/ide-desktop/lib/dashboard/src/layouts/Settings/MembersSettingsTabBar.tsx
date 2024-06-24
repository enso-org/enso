/** @file Button bar for managing organization members. */
import * as React from 'react'

import type * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as paywallComponents from '#/components/Paywall'

import InviteUsersModal from '#/modals/InviteUsersModal'

// =============================
// === MembersSettingsTabBar ===
// =============================

/**
 *
 */
export interface MembersSettingsTabBarProps {
  readonly seatsLeft: number | null
  readonly seatsTotal: number
  readonly feature: billingHooks.PaywallFeatureName
}

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar(props: MembersSettingsTabBarProps) {
  const { seatsLeft, seatsTotal, feature } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.ButtonGroup>
      <ariaComponents.DialogTrigger>
        <ariaComponents.Button variant="bar" rounded="full" size="medium">
          {getText('inviteMembers')}
        </ariaComponents.Button>

        <InviteUsersModal />
      </ariaComponents.DialogTrigger>

      {seatsLeft != null && (
        <div className="flex gap-1">
          <ariaComponents.Text>
            {seatsLeft <= 0 ? getText('noSeatsLeft') : getText('seatsLeft', seatsLeft, seatsTotal)}
          </ariaComponents.Text>

          <paywallComponents.PaywallDialogButton
            feature={feature}
            variant="link"
            showIcon={false}
          />
        </div>
      )}
    </ariaComponents.ButtonGroup>
  )
}
