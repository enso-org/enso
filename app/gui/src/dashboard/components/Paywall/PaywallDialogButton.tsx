/** @file A button that opens a paywall dialog when clicked. */
import { Dialog, type DialogTriggerProps } from '#/components/Dialog'
import * as React from 'react'
import * as components from './components'
import * as paywallDialog from './PaywallDialog'

/** Props for a {@link PaywallDialogButton}. */
export type PaywallDialogButtonProps<IconType extends string> =
  components.PaywallButtonProps<IconType> & {
    readonly dialogProps?: paywallDialog.PaywallDialogProps
    readonly dialogTriggerProps?: DialogTriggerProps
  }

/** A button that opens a paywall dialog when clicked */
export function PaywallDialogButton<IconType extends string>(
  props: PaywallDialogButtonProps<IconType>,
): React.JSX.Element {
  const { feature, dialogProps, dialogTriggerProps, ...buttonProps } = props

  return (
    <Dialog.Trigger {...dialogTriggerProps}>
      <components.PaywallButton feature={feature} {...buttonProps} />

      <paywallDialog.PaywallDialog feature={feature} {...dialogProps} />
    </Dialog.Trigger>
  )
}
