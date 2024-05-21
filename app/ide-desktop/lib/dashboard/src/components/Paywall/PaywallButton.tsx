/**
 * @file
 *
 * PaywallButton component
 */

import * as React from 'react'

import PaywallBlocked from 'enso-assets/lock.svg'

import * as button from '#/components/AriaComponents'

/**
 * Props of the PaywallButton component
 */
export type PaywallButtonProps = button.ButtonProps & {}

/**
 * PaywallButton component
 */
export function PaywallButton(props: PaywallButtonProps) {
  const { size = 'medium', variant = 'primary' } = props

  return (
    <button.Button
      variant={variant}
      size={size}
      rounding="full"
      icon={PaywallBlocked}
      iconPosition="end"
    />
  )
}
