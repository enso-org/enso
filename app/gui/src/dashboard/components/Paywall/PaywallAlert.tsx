/**
 * @file
 *
 * A paywall alert.
 */

import * as React from 'react'

import clsx from 'clsx'

import LockIcon from '#/assets/lock.svg'

import type * as billingHooks from '#/hooks/billing'

import * as ariaComponents from '#/components/AriaComponents'
import * as paywall from '#/components/Paywall'
import SvgMask from '#/components/SvgMask'

/** Props for {@link PaywallAlert}. */
export interface PaywallAlertProps extends Omit<ariaComponents.AlertProps, 'children'> {
  readonly feature: billingHooks.PaywallFeatureName
  readonly label: string
  readonly showUpgradeButton?: boolean
  readonly upgradeButtonProps?: Omit<paywall.UpgradeButtonProps, 'feature'>
}

/** A paywall alert. */
export function PaywallAlert(props: PaywallAlertProps) {
  const {
    label,
    showUpgradeButton = true,
    feature,
    upgradeButtonProps,
    className,
    ...alertProps
  } = props

  return (
    <ariaComponents.Alert
      variant="outline"
      size="small"
      rounded="xlarge"
      className={clsx('border border-primary/20', className)}
      {...alertProps}
    >
      <div className="flex items-center gap-2">
        <SvgMask src={LockIcon} className="h-5 w-5 flex-none text-primary" />

        <ariaComponents.Text>
          {label}{' '}
          {showUpgradeButton && (
            <paywall.UpgradeButton
              feature={feature}
              variant="link"
              size="small"
              {...upgradeButtonProps}
            />
          )}
        </ariaComponents.Text>
      </div>
    </ariaComponents.Alert>
  )
}
