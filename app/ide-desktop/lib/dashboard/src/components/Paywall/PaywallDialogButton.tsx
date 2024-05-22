/**
 * @file
 *
 * A button that opens a paywall dialog when clicked.
 */

import * as React from 'react'

import type * as billingHooks from '#/hooks/billing'

import * as ariaComponents from '#/components/AriaComponents'

import * as components from './components'
import * as paywallDialog from './PaywallDialog'

/**
 * Props for a {@link PaywallDialogButton}.
 */
export type PaywallDialogButtonProps = Omit<ariaComponents.ButtonProps, 'variant'> & {
  readonly variant?: ariaComponents.ButtonProps['variant']
  readonly feature: billingHooks.PaywallFeatureName
  readonly dialogProps?: paywallDialog.PaywallDialogProps
  readonly dialogTriggerProps?: ariaComponents.DialogTriggerProps
}

/**
 * A button that opens a paywall dialog when clicked
 */
export function PaywallDialogButton(props: PaywallDialogButtonProps) {
  const { feature, dialogProps, dialogTriggerProps, ...buttonProps } = props

  return (
    <ariaComponents.DialogTrigger {...dialogTriggerProps}>
      <components.PaywallButton feature={feature} {...buttonProps} />

      <paywallDialog.PaywallDialog feature={feature} {...dialogProps} />
    </ariaComponents.DialogTrigger>
  )
}
