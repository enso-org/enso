/** @file An entry in a menu. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import * as aria from '#/components/aria'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import UnstyledButton from '#/components/styled/UnstyledButton'
import SvgMask from '#/components/SvgMask'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
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
export default function MenuEntry(props: MenuEntryProps) {
  const { hidden = false, action, label: labelRaw, isDisabled = false, title } = props
  const { isContextMenuEntry = false, doAction } = props
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
  const label = labelRaw ?? info.name
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
    <UnstyledButton
      isDisabled={isDisabled}
      className="group flex w-full rounded-menu-entry"
      onPress={doAction}
    >
      <div
        className={`rounded-inherit flex h-row grow place-content-between items-center p-menu-entry text-left selectable hover:bg-hover-bg disabled:bg-transparent group-enabled:active ${
          isContextMenuEntry ? 'px-context-menu-entry-x' : ''
        }`}
      >
        <div title={title} className="flex items-center gap-menu-entry whitespace-nowrap">
          <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="size-icon" />
          <aria.Text slot="label">{label}</aria.Text>
        </div>
        <KeyboardShortcut action={action} />
      </div>
    </UnstyledButton>
  )
}
