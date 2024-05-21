/**
 * @file
 *
 * A screen that shows a paywall.
 */

import * as React from 'react'

import * as tw from 'tailwind-merge'

import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

import { PaywallBulletPoints } from './PaywallBulletPoints'

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

  const isEnterprise = level === billingHooks.PAYWALL_LEVELS.enterprise

  return (
    <div className={tw.twMerge('flex flex-col items-start', className)}>
      <div className="mb-1 flex flex-col items-center justify-center">
        <div className="flex w-full items-center gap-1 text-sm font-normal">
          <SvgMask src={LockIcon} role="presentation" className="h-4 w-4" />
          {getText('paywallAvailabilityLevel', levelLabel)}
        </div>
      </div>

      <aria.Text elementType="h2" className="text-2xl font-bold text-gray-900">
        {getText('paywallScreenTitle')}
      </aria.Text>

      <PaywallBulletPoints bulletPointsTextId={bulletPointsTextId} className="mb-6 mt-4" />

      <p className="text-sm font-normal text-gray-600">
        {getText('paywallScreenDescription', levelLabel)}
      </p>

      <ariaComponents.Button
        variant="primary"
        size="medium"
        className="mt-3"
        href={appUtils.SUBSCRIBE_PATH + '?plan=' + level.name}
      >
        {isEnterprise ? getText('contactSales') : getText('upgradeTo', levelLabel)}
      </ariaComponents.Button>
    </div>
  )
}
