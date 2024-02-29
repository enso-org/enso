/** @file An entry in a menu. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
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
  readonly disabled?: boolean
  readonly title?: string
  readonly paddingClassName?: string
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const { hidden = false, action, label, disabled = false, title, paddingClassName } = props
  const { doAction } = props
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
  React.useEffect(() => {
    // This is slower (but more convenient) than registering every shortcut in the context menu
    // at once.
    if (disabled) {
      return
    } else {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        [action]: doAction,
      })
    }
  }, [disabled, inputBindings, action, doAction])

  return hidden ? null : (
    <button
      disabled={disabled}
      title={title}
      className={`flex items-center place-content-between h-8 disabled:bg-transparent rounded-lg text-left disabled:opacity-50 hover:bg-black/10 ${
        paddingClassName ?? 'px-3 py-1'
      }`}
      onClick={event => {
        event.stopPropagation()
        doAction()
      }}
    >
      <div className="flex items-center gap-3">
        <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="w-4 h-4" />
        {label ?? getText(`${action}Shortcut`)}
      </div>
      <KeyboardShortcut action={action} />
    </button>
  )
}
