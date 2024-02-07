/** @file An entry in a menu. */
import * as React from 'react'

import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'
import * as textProvider from '#/providers/TextProvider'

import KeyboardShortcut from '#/components/dashboard/keyboardShortcut'
import SvgMask from '#/components/SvgMask'

import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
  readonly hidden?: boolean
  readonly action: shortcutManagerModule.KeyboardAction
  /** When true, the button is not clickable. */
  readonly disabled?: boolean
  readonly title?: string
  readonly paddingClassName?: string
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const { hidden = false, action, disabled = false, title, paddingClassName, doAction } = props
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const { getText } = textProvider.useText()
  const info = shortcutManager.keyboardShortcutInfo[action]
  React.useEffect(() => {
    // This is slower than registering every shortcut in the context menu at once.
    if (!disabled) {
      return shortcutManager.registerKeyboardHandlers({
        [action]: doAction,
      })
    } else {
      return
    }
  }, [disabled, shortcutManager, action, doAction])
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
        <SvgMask
          style={{
            width: shortcutManagerModule.ICON_SIZE_PX,
            height: shortcutManagerModule.ICON_SIZE_PX,
          }}
          src={info.icon}
          className={info.colorClass}
        />
        {getText(`${action}Shortcut`)}
      </div>
      <KeyboardShortcut action={action} />
    </button>
  )
}
