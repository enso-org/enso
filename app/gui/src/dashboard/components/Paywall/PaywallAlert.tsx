/** @file A paywall alert. */
import LockIcon from '#/assets/lock.svg'
import { Alert, Text, type AlertProps } from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import type { PaywallFeatureName } from '#/hooks/billing'
import { twJoin } from '#/utilities/tailwindMerge'
import type { JSX } from 'react'
import { UpgradeButton, type UpgradeButtonProps } from './UpgradeButton'

/** Props for {@link PaywallAlert}. */
export interface PaywallAlertProps<IconType extends string> extends Omit<AlertProps, 'children'> {
  readonly feature: PaywallFeatureName
  readonly label: string
  readonly showUpgradeButton?: boolean
  readonly upgradeButtonProps?: Omit<UpgradeButtonProps<IconType>, 'feature'>
}

/** A paywall alert. */
export function PaywallAlert<IconType extends string>(
  props: PaywallAlertProps<IconType>,
): JSX.Element {
  const {
    label,
    showUpgradeButton = true,
    feature,
    upgradeButtonProps,
    className,
    ...alertProps
  } = props

  return (
    <Alert
      variant="outline"
      size="small"
      rounded="xlarge"
      className={twJoin('border border-primary/20', className)}
      {...alertProps}
    >
      <div className="flex items-center gap-2">
        <SvgMask src={LockIcon} className="h-5 w-5 flex-none text-primary" />

        <Text>
          {label}{' '}
          {showUpgradeButton && (
            <UpgradeButton feature={feature} variant="link" size="small" {...upgradeButtonProps} />
          )}
        </Text>
      </div>
    </Alert>
  )
}
