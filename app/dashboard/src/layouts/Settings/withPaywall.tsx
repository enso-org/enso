/**
 * @file
 *
 * This file contains a higher-order component that wraps a component in a paywall.
 * The paywall is shown if the user's plan does not include the feature.
 * The feature is determined by the `isFeatureUnderPaywall` hook.
 */

import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'

import * as paywallComponents from '#/components/Paywall'

/**
 * Props for the `withPaywall` HOC.
 */
export interface PaywallSettingsLayoutProps {
  readonly feature: billingHooks.PaywallFeatureName
  readonly className?: string | undefined
}

const PAYWALL_LAYOUT_STYLES = twv.tv({
  base: 'mt-1',
})

/**
 * A layout that shows a paywall for a feature.
 */
export function PaywallSettingsLayout(props: PaywallSettingsLayoutProps) {
  const { feature, className } = props

  return (
    <div className={PAYWALL_LAYOUT_STYLES({ className })}>
      <paywallComponents.PaywallScreen feature={feature} />
    </div>
  )
}

/**
 * Wraps a component in a paywall.
 * The paywall is shown if the user's plan does not include the feature.
 * The feature is determined by the `isFeatureUnderPaywall` hook.
 */
export function withPaywall<P extends Record<string, unknown>>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Component: React.ComponentType<P>,
  props: PaywallSettingsLayoutProps
) {
  const { feature, className } = props

  return function WithPaywall(componentProps: P) {
    const { user } = authProvider.useFullUserSession()

    const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
    const showPaywall = isFeatureUnderPaywall(feature)

    if (showPaywall) {
      return <PaywallSettingsLayout feature={feature} className={className} />
    } else {
      return <Component {...componentProps} />
    }
  }
}
