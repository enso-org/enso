/**
 * @file
 *
 * A button that links to the upgrade page.
 */
import { getContactSalesURL, getUpgradeURL } from '#/appUtils'
import { Button, type ButtonProps } from '#/components/AriaComponents'
import * as billingHooks from '#/hooks/billing'
import { useText } from '#/providers/TextProvider'

/**
 * Props for an {@link UpgradeButton}.
 */
// eslint-disable-next-line no-restricted-syntax
export type UpgradeButtonProps = Omit<ButtonProps, 'variant'> & {
  readonly feature: billingHooks.PaywallFeatureName
  readonly variant?: ButtonProps['variant']
}

/**
 * A button that links to the upgrade page.
 */
export function UpgradeButton(props: UpgradeButtonProps) {
  const {
    feature,
    variant,
    href,
    size = 'medium',
    rounded = 'xlarge',
    children,
    ...buttonProps
  } = props
  const { getText } = useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { level } = getFeature(feature)
  const levelLabel = getText(level.label)

  const isEnterprise = level === billingHooks.PAYWALL_LEVELS.enterprise
  const child =
    children ?? (isEnterprise ? getText('contactSales') : getText('upgradeTo', levelLabel))

  return (
    <Button
      variant={variant ?? VARIANT_BY_LEVEL[level.name]}
      size={size}
      rounded={rounded}
      href={isEnterprise ? getContactSalesURL() : href ?? getUpgradeURL(level.name)}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    >
      {child}
    </Button>
  )
}

const VARIANT_BY_LEVEL: Record<billingHooks.PaywallLevelName, ButtonProps['variant']> = {
  free: 'primary',
  enterprise: 'primary',
  solo: 'outline',
  team: 'submit',
}
