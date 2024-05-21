/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'

import * as paywallComponents from '#/components/Paywall'

import * as components from './components'

// =============================
// === UserGroupsSettingsTab ===
// =============================

/** Settings tab for viewing and editing organization members. */
export default function UserGroupsSettingsTab() {
  const { user } = authProvider.useFullUserSession()

  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
  const showPaywall = isFeatureUnderPaywall('userGroups')

  if (!showPaywall) {
    return (
      <div className="mt-1">
        <paywallComponents.PaywallScreen feature="userGroups" />
      </div>
    )
  } else {
    return <components.UserGroupsSettingsTabContent />
  }
}
