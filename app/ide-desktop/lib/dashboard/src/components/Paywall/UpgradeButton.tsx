/**
 * @file
 *
 * A button that links to the upgrade page.
 */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * Props for an {@link UpgradeButton}.
 */
export type UpgradeButtonProps = Omit<ariaComponents.ButtonProps, 'variant'> & {
  readonly feature: billingHooks.PaywallFeatureName
  readonly variant?: ariaComponents.ButtonProps['variant']
}

/**
 * A button that links to the upgrade page.
 */
export function UpgradeButton(props: UpgradeButtonProps) {
  const { feature, variant = 'primary', href, size = 'medium', children, ...buttonProps } = props
  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { level } = getFeature(feature)
  const levelLabel = getText(level.label)

  const isEnterprise = level === billingHooks.PAYWALL_LEVELS.enterprise
  const child =
    children ?? (isEnterprise ? getText('contactSales') : getText('upgradeTo', levelLabel))

  return (
    <ariaComponents.Button
      variant={variant}
      size={size}
      href={
        isEnterprise ? appUtils.getContactSalesURL() : href ?? appUtils.getUpgradeURL(level.name)
      }
      {...buttonProps}
    >
      {child}
    </ariaComponents.Button>
  )
}
