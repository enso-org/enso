/** @file A styled button representing a tab on a sidebar. */
import * as React from 'react'

import * as ariaComponent from '#/components/AriaComponents'

// ========================
// === SidebarTabButton ===
// ========================

/** Props for a {@link SidebarTabButton}. */
export interface SidebarTabButtonProps {
  readonly id: string
  readonly isDisabled?: boolean
  readonly autoFocus?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  readonly icon: string
  readonly label: string
  readonly onPress: ariaComponent.ButtonProps['onPress']
}

/** A styled button representing a tab on a sidebar. */
export default function SidebarTabButton(props: SidebarTabButtonProps) {
  const { isDisabled = false, active = false, icon, label, onPress } = props

  return (
    <ariaComponent.Button
      onPress={onPress}
      icon={icon}
      variant="ghost-fading"
      loaderPosition="icon"
      size="medium"
      isDisabled={isDisabled}
      rounded="full"
      className={`${active ? 'bg-white opacity-100' : ''} font-medium`}
    >
      {label}
    </ariaComponent.Button>
  )
}
