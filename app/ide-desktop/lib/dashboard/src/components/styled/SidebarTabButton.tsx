/** @file A styled button representing a tab on a sidebar. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

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
  readonly isPending?: boolean
}

/** A styled button representing a tab on a sidebar. */
export default function SidebarTabButton(props: SidebarTabButtonProps) {
  const {
    isDisabled = false,
    autoFocus = false,
    active = false,
    icon,
    label,
    onPress,
    isPending = false,
  } = props

  return (
    <ariaComponents.Button
      size="custom"
      variant="custom"
      autoFocus={autoFocus}
      onPress={onPress}
      isDisabled={isDisabled}
      className={tailwindMerge.twMerge('relative rounded-full', active && 'focus-default')}
    >
      <div
        className={tailwindMerge.twMerge(
          'button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame',
          active && 'disabled bg-selected-frame active'
        )}
      >
        {active && isPending ? (
          <StatelessSpinner state={statelessSpinner.SpinnerState.loadingMedium} size={16} />
        ) : (
          <SvgMask src={icon} />
        )}
        <aria.Text className="text">{label}</aria.Text>
      </div>
    </ariaComponents.Button>
  )
}
