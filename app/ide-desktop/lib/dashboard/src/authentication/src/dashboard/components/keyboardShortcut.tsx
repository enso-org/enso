/** @file A visual representation of a keyboard shortcut. */
import * as React from 'react'

import CommandKeyIcon from 'enso-assets/command_key.svg'
import CtrlKeyIcon from 'enso-assets/ctrl_key.svg'
import OptionKeyIcon from 'enso-assets/option_key.svg'
import ShiftKeyIcon from 'enso-assets/shift_key.svg'
import WindowsKeyIcon from 'enso-assets/windows_key.svg'

import * as detect from 'enso-common/src/detect'
import * as shortcuts from '../shortcuts'
import SvgMask from '../../authentication/components/svgMask'

// ========================
// === KeyboardShortcut ===
// ========================

/** Icons for modifier keys (if they exist). */
const MODIFIER_MAPPINGS: Partial<Record<shortcuts.ModifierKey, React.ReactNode>> =
    detect.platform() === detect.Platform.macOS
        ? // The names are intentionally not in `camelCase`.
          /* eslint-disable @typescript-eslint/naming-convention */
          {
              Meta: <SvgMask key="Meta" src={CommandKeyIcon} />,
              Shift: <SvgMask key="Shift" src={ShiftKeyIcon} />,
              Alt: <SvgMask key="Alt" src={OptionKeyIcon} />,
              Ctrl: <SvgMask key="Ctrl" src={CtrlKeyIcon} />,
          }
        : {
              // TODO[sb]: These are required, otherwise the entry for "New Data Connector" will
              // span across two lines. This should be replaced with proper Windows equivalents.
              Meta: <SvgMask key="Meta" src={WindowsKeyIcon} />,
              Shift: <SvgMask key="Shift" src={ShiftKeyIcon} />,
              Alt: <SvgMask key="Alt" src={OptionKeyIcon} />,
              Ctrl: <SvgMask key="Ctrl" src={CommandKeyIcon} />,
          }
/* eslint-enable @typescript-eslint/naming-convention */

/** Props for a {@link KeyboardShortcut} */
export interface KeyboardShortcutProps {
    action: shortcuts.KeyboardAction
}

/** A visual representation of a keyboard shortcut. */
export default function KeyboardShortcut(props: KeyboardShortcutProps) {
    const { action } = props
    const shortcut = shortcuts.SHORTCUT_REGISTRY.keyboardShortcuts[action][0]
    if (shortcut == null) {
        return null
    } else {
        return (
            <div className="flex items-center h-6 gap-0.5">
                {shortcuts.getModifierKeysOfShortcut(shortcut).map(
                    modifier =>
                        MODIFIER_MAPPINGS[modifier] ?? (
                            <span key={modifier} className="leading-170 h-6 py-px">
                                {modifier}
                            </span>
                        )
                )}
                <span className="leading-170 h-6 py-px">{shortcut.key}</span>
            </div>
        )
    }
}
