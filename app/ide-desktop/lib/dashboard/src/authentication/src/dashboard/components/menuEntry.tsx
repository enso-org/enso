/** @file An entry in a menu. */
import * as React from 'react'

import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'

import KeyboardShortcut from './keyboardShortcut'
import SvgMask from '../../authentication/components/svgMask'

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
    hidden?: boolean
    action: shortcutsModule.KeyboardAction
    /** When true, the button is not clickable. */
    disabled?: boolean
    title?: string
    paddingClassName?: string
    doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
    const { hidden = false, action, disabled = false, title, paddingClassName, doAction } = props
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const info = shortcuts.keyboardShortcutInfo[action]
    React.useEffect(() => {
        // This is slower than registering every shortcut in the context menu at once.
        if (!disabled) {
            return shortcuts.registerKeyboardHandlers({
                [action]: doAction,
            })
        } else {
            return
        }
    }, [disabled, shortcuts, action, doAction])
    return hidden ? null : (
        <button
            disabled={disabled}
            title={title}
            className={`flex h-8 place-content-between items-center rounded-lg text-left hover:bg-black-a10 disabled:bg-transparent disabled:opacity-50 ${
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
                        width: shortcutsModule.ICON_SIZE_PX,
                        height: shortcutsModule.ICON_SIZE_PX,
                    }}
                    src={info.icon}
                    className={info.colorClass}
                />
                {info.name}
            </div>
            <KeyboardShortcut action={action} />
        </button>
    )
}
