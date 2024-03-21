/** @file An entry in a menu. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import * as aria from '#/components/aria'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import SvgMask from '#/components/SvgMask'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
  readonly focusRing?: boolean
  readonly hidden?: boolean
  readonly action: inputBindings.DashboardBindingKey
  /** Overrides the text for the menu entry. */
  readonly label?: string
  /** When true, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly title?: string
  readonly isContextMenuEntry?: boolean
  readonly doAction: () => void
}

/** An item in a menu. */
function MenuEntry(props: MenuEntryProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const { focusRing = false, hidden = false, action, label, isDisabled = false, title } = props
  const { isContextMenuEntry = false, doAction } = props
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
  React.useEffect(() => {
    // This is slower (but more convenient) than registering every shortcut in the context menu
    // at once.
    if (isDisabled) {
      return
    } else {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        [action]: doAction,
      })
    }
  }, [isDisabled, inputBindings, action, doAction])

  return hidden ? null : (
    <aria.Button
      ref={ref}
      isDisabled={isDisabled}
      className={`flex h-row place-content-between items-center rounded-menu-entry p-menu-entry text-left selectable hover:bg-hover-bg enabled:active disabled:bg-transparent ${
        isContextMenuEntry ? 'px-context-menu-entry-x' : ''
      } ${focusRing ? 'focus-ring' : ''}`}
      onPress={doAction}
    >
      <div title={title} className="flex items-center gap-menu-entry whitespace-nowrap">
        <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="size-icon" />
        {label ?? info.name}
      </div>
      <KeyboardShortcut action={action} />
    </aria.Button>
  )
}

export default React.forwardRef(MenuEntry)
