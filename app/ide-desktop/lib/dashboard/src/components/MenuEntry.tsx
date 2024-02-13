/** @file An entry in a menu. */
import * as React from 'react'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import KeyboardShortcut from '#/components/dashboard/keyboardShortcut'
import SvgMask from '#/components/SvgMask'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
  readonly hidden?: boolean
  readonly action: inputBindings.DashboardBindingKey
  /** When true, the button is not clickable. */
  readonly disabled?: boolean
  readonly title?: string
  readonly paddingClassName?: string
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const { hidden = false, action, disabled = false, title, paddingClassName, doAction } = props
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = shortcutManager.keyboardShortcutInfo[action]
  React.useEffect(() => {
    // This is slower than registering every shortcut in the context menu at once.
    if (!disabled) {
      return inputBindings.attach(sanitizedEventTargets.document, 'keydown', {
        [action]: doAction,
      })
    } else {
      return
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
        <SvgMask src={info.icon} className={`w-4 h-4 ${info.colorClass}`} />
        {info.name}
      </div>
      <KeyboardShortcut action={action} />
    </button>
  )
}
