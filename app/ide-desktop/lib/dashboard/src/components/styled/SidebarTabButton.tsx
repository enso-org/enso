/** @file A styled button representing a tab on a sidebar. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import UnstyledButton from '#/components/styled/UnstyledButton'
import SvgMask from '#/components/SvgMask'

// ========================
// === SidebarTabButton ===
// ========================

/** Props for a {@link SidebarTabButton}. */
export interface SidebarTabButtonProps {
  readonly id: string
  readonly autoFocus?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  readonly icon: string
  readonly label: string
  readonly onPress: (event: aria.PressEvent) => void
}

/** A styled button representing a tab on a sidebar. */
export default function SidebarTabButton(props: SidebarTabButtonProps) {
  const { autoFocus = false, active = false, icon, label, onPress } = props

  return (
    <UnstyledButton
      autoFocus={autoFocus}
      onPress={onPress}
      className={`rounded-full ${active ? 'focus-default' : ''}`}
    >
      <div
        className={`button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame ${active ? 'disabled bg-selected-frame active' : ''}`}
      >
        <SvgMask src={icon} />
        <span className="text">{label}</span>
      </div>
    </UnstyledButton>
  )
}
