/**
 * @file A lock icon with a label indicating the paywall level required to access a feature.
 */
import * as tw from 'tailwind-merge'

import LockIcon from 'enso-assets/lock.svg'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

/**
 * Props for a {@link PaywallLock}.
 */
export interface PaywallLockProps {
  readonly feature: billingHooks.PaywallFeatureName
  readonly className?: string
}

/**
 * A lock icon with a label indicating the paywall level required to access a feature.
 */
export function PaywallLock(props: PaywallLockProps) {
  const { feature, className } = props
  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { level } = getFeature(feature)
  const levelLabel = getText(level.label)

  return (
    <div className={tw.twMerge('flex w-full items-center gap-1 text-sm font-normal', className)}>
      <SvgMask src={LockIcon} role="presentation" className="h-4 w-4" />
      {getText('paywallAvailabilityLevel', levelLabel)}
    </div>
  )
}
