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

/** Props for an {@link UpgradeButton}. */
export type UpgradeButtonProps<IconType extends string = string> = Omit<
  ariaComponents.ButtonProps<IconType>,
  'variant'
> & {
  readonly feature: billingHooks.PaywallFeatureName
  readonly variant?: ariaComponents.ButtonProps<IconType>['variant']
}

/** A button that links to the upgrade page. */
export function UpgradeButton<IconType extends string>(
  props: UpgradeButtonProps<IconType>,
): React.JSX.Element {
  const {
    feature,
    variant,
    href,
    size = 'medium',
    rounded = 'xlarge',
    children,
    ...buttonProps
  } = props
  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { level } = getFeature(feature)
  const levelLabel = getText(level.label)

  const isEnterprise = level === billingHooks.PAYWALL_LEVELS.enterprise
  const child =
    children ?? (isEnterprise ? getText('contactSales') : getText('upgradeTo', levelLabel))

  return (
    <ariaComponents.Button
      variant={variant ?? VARIANT_BY_LEVEL[level.name]}
      size={size}
      rounded={rounded}
      href={
        isEnterprise ? appUtils.getContactSalesURL() : (href ?? appUtils.getUpgradeURL(level.name))
      }
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    >
      {child}
    </ariaComponents.Button>
  )
}

const VARIANT_BY_LEVEL: Record<
  billingHooks.PaywallLevelName,
  ariaComponents.ButtonProps<string>['variant']
> = {
  free: 'primary',
  enterprise: 'primary',
  solo: 'accent',
  team: 'submit',
}
