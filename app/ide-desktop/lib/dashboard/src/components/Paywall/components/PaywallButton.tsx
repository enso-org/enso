/**
 * @file
 *
 * PaywallDialogButton component
 */
import * as React from 'react'

import PaywallBlocked from 'enso-assets/lock.svg'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * Props of the PaywallDialogButton component
 */
export type PaywallButtonProps = Omit<ariaComponents.ButtonProps, 'variant'> & {
  readonly feature: billingHooks.PaywallFeatureName
  readonly variant?: ariaComponents.ButtonProps['variant']
  readonly iconOnly?: boolean
}

/**
 * PaywallDialogButton component
 */
export function PaywallButton(props: PaywallButtonProps) {
  const { feature, iconOnly = false, children, ...buttonProps } = props

  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { level } = getFeature(feature)
  const levelLabel = getText(level.label)

  const showChildren = !iconOnly
  const childrenContent = children ?? getText('upgradeTo', levelLabel)

  return (
    <ariaComponents.Button
      variant="primary"
      size="medium"
      icon={PaywallBlocked}
      iconPosition="end"
      tooltip={getText('paywallScreenDescription', levelLabel)}
      {...buttonProps}
    >
      {showChildren && childrenContent}
    </ariaComponents.Button>
  )
}
