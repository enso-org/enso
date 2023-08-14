/** @file An entry in a context menu. */
import * as React from 'react'

import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'

import KeyboardShortcut from './keyboardShortcut'
import SvgMask from '../../authentication/components/svgMask'

// ========================
// === ContextMenuEntry ===
// ========================

/** Props for a {@link ContextMenuEntry}. */
export interface ContextMenuEntryProps {
    hidden?: boolean
    action: shortcutsModule.KeyboardAction
    disabled?: boolean
    title?: string
    doAction: () => void
}

/** An item in a `ContextMenu`. */
export default function ContextMenuEntry(props: ContextMenuEntryProps) {
    const { hidden = false, action, disabled = false, title, doAction } = props
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
            className="flex items-center place-content-between h-8 px-3 py-1 hover:bg-black-a10 disabled:bg-transparent rounded-lg text-left disabled:opacity-50"
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
