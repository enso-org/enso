/**
 * @file
 *
 * A styled button that shows that a feature is behind a paywall
 */
import * as React from 'react'

import PaywallBlocked from '#/assets/lock.svg'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/** Props for {@link PaywallButton}. */
export type PaywallButtonProps = ariaComponents.ButtonProps & {
  readonly feature: billingHooks.PaywallFeatureName
  readonly iconOnly?: boolean
  readonly showIcon?: boolean
}

/** A styled button that shows that a feature is behind a paywall */
export function PaywallButton(props: PaywallButtonProps) {
  const { feature, iconOnly = false, showIcon = true, children, ...buttonProps } = props

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
      icon={showIcon ? PaywallBlocked : null}
      iconPosition="end"
      tooltip={getText('paywallScreenDescription', levelLabel)}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    >
      {showChildren && childrenContent}
    </ariaComponents.Button>
  )
}
