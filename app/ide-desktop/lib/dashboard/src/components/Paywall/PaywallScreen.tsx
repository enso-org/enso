/**
 * @file
 *
 * A screen that shows a paywall.
 */

import * as React from 'react'

import * as tw from 'tailwind-merge'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import * as components from './components'
import * as upgradeButton from './UpgradeButton'

/**
 * Props for a {@link PaywallScreen}.
 */
export interface PaywallScreenProps {
  readonly feature: billingHooks.PaywallFeatureName
  readonly className?: string
}

/**
 * A screen that shows a paywall.
 */
export function PaywallScreen(props: PaywallScreenProps) {
  const { feature, className } = props
  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { bulletPointsTextId, level } = getFeature(feature)
  const levelLabel = getText(level.label)

  return (
    <div className={tw.twMerge('flex flex-col items-start', className)}>
      <components.PaywallLock feature={feature} className="mb-1" />

      <aria.Text elementType="h2" className="text-2xl font-bold text-gray-900">
        {getText('paywallScreenTitle')}
      </aria.Text>

      <p className="mt-2 text-base font-normal text-gray-600">
        {getText('paywallScreenDescription', levelLabel)}
      </p>

      <components.PaywallBulletPoints
        bulletPointsTextId={bulletPointsTextId}
        className="mb-6 mt-4"
      />

      <upgradeButton.UpgradeButton feature={feature} />
    </div>
  )
}
