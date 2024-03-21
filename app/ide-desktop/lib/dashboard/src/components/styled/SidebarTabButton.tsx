/** @file A styled button representing a tab on a sidebar. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
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
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <aria.MenuItem>
      <FocusRing>
        <aria.Button
          autoFocus={autoFocus}
          className={`button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame ${active ? 'disabled bg-selected-frame active' : ''}`}
          onPress={onPress}
          onKeyDown={handleFocusMove}
        >
          <SvgMask src={icon} />
          <span className="text">{label}</span>
        </aria.Button>
      </FocusRing>
    </aria.MenuItem>
  )
}
