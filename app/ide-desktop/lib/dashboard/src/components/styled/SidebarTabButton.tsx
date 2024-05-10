/** @file A styled button representing a tab on a sidebar. */
import * as React from 'react'

import * as aria from '#/components/aria'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

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
  readonly onPress: (event: aria.PressEvent) => void
}

/** A styled button representing a tab on a sidebar. */
export default function SidebarTabButton(props: SidebarTabButtonProps) {
  const { isDisabled = false, autoFocus = false, active = false, icon, label, onPress } = props

  return (
    <UnstyledButton
      autoFocus={autoFocus}
      onPress={onPress}
      isDisabled={isDisabled}
      className={`relative rounded-full ${active ? 'focus-default' : ''}`}
    >
      <div
        className={`button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame ${active ? 'disabled bg-selected-frame active' : ''}`}
      >
        <SvgMask src={icon} />
        <aria.Text className="text">{label}</aria.Text>
      </div>
    </UnstyledButton>
  )
}
