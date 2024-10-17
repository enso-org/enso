/**
 * @file
 *
 * A screen that shows a paywall.
 */

import * as React from 'react'

import * as tw from 'tailwind-merge'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as components from './components'
import * as upgradeButton from './UpgradeButton'

/** Props for a {@link PaywallScreen}. */
export interface PaywallScreenProps {
  readonly feature: billingHooks.PaywallFeatureName
  readonly className?: string
}

/** A screen that shows a paywall. */
export function PaywallScreen(props: PaywallScreenProps) {
  const { feature, className } = props
  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { bulletPointsTextId, descriptionTextId } = getFeature(feature)

  return (
    <div className={tw.twMerge('flex flex-col items-start', className)}>
      <components.PaywallLock feature={feature} />

      <ariaComponents.Text.Heading level="2">
        {getText('paywallScreenTitle')}
      </ariaComponents.Text.Heading>

      <ariaComponents.Text balance variant="subtitle" className="mt-1 max-w-[720px]">
        {getText(descriptionTextId)}
      </ariaComponents.Text>

      <components.PaywallBulletPoints bulletPointsTextId={bulletPointsTextId} className="my-3" />

      <upgradeButton.UpgradeButton feature={feature} className="mt-0.5 min-w-36" />
    </div>
  )
}
