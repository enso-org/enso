/** @file A visual representation of a keyboard shortcut. */
import * as React from 'react'

import CommandKeyIcon from 'enso-assets/command_key.svg'
import CtrlKeyIcon from 'enso-assets/ctrl_key.svg'
import OptionKeyIcon from 'enso-assets/option_key.svg'
import ShiftKeyIcon from 'enso-assets/shift_key.svg'
import WindowsKeyIcon from 'enso-assets/windows_key.svg'
import * as detect from 'enso-common/src/detect'

import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import SvgMask from '#/components/SvgMask'

import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// ========================
// === KeyboardShortcut ===
// ========================

/** The size (both width and height) of key icons. */
const ICON_SIZE_PX = 13

const ICON_STYLE = { width: ICON_SIZE_PX, height: ICON_SIZE_PX }

/** Icons for modifier keys (if they exist). */
const MODIFIER_MAPPINGS: Readonly<
  Record<detect.Platform, Partial<Record<shortcutManagerModule.ModifierKey, React.ReactNode>>>
> = {
  // The names are intentionally not in `camelCase`, as they are case-sensitive.
  /* eslint-disable @typescript-eslint/naming-convention */
  [detect.Platform.macOS]: {
    Meta: <SvgMask style={ICON_STYLE} key="Meta" src={CommandKeyIcon} />,
    Shift: <SvgMask style={ICON_STYLE} key="Shift" src={ShiftKeyIcon} />,
    Alt: <SvgMask style={ICON_STYLE} key="Alt" src={OptionKeyIcon} />,
    Ctrl: <SvgMask style={ICON_STYLE} key="Ctrl" src={CtrlKeyIcon} />,
  },
  [detect.Platform.windows]: {
    Meta: <SvgMask style={ICON_STYLE} key="Meta" src={WindowsKeyIcon} />,
  },
  [detect.Platform.linux]: {
    Meta: (
      <span key="Meta" className="leading-170 h-6 py-px">
        Super
      </span>
    ),
  },
  [detect.Platform.unknown]: {
    // Assume the system is Unix-like and calls the key that triggers `event.metaKey`
    // the "Super" key.
    Meta: (
      <span key="Meta" className="leading-170 h-6 py-px">
        Super
      </span>
    ),
  },
  /* eslint-enable @typescript-eslint/naming-convention */
}

/** Props for a {@link KeyboardShortcut}, specifying the keyboard action. */
export interface KeyboardShortcutActionProps {
  readonly action: shortcutManagerModule.KeyboardAction
}

/** Props for a {@link KeyboardShortcut}, specifying the shortcut info. */
export interface KeyboardShortcutShortcutProps {
  readonly shortcut: shortcutManagerModule.KeyboardShortcut
}

/** Props for a {@link KeyboardShortcut}. */
export type KeyboardShortcutProps = KeyboardShortcutActionProps | KeyboardShortcutShortcutProps

/** A visual representation of a keyboard shortcut. */
export default function KeyboardShortcut(props: KeyboardShortcutProps) {
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const shortcut =
    'shortcut' in props ? props.shortcut : shortcutManager.keyboardShortcuts[props.action][0]
  if (shortcut == null) {
    return null
  } else {
    return (
      <div className={`flex items-center h-6 ${detect.isOnMacOS() ? 'gap-0.5' : 'gap-0.75'}`}>
        {shortcutManagerModule.getModifierKeysOfShortcut(shortcut).map(
          modifier =>
            MODIFIER_MAPPINGS[detect.platform()][modifier] ?? (
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
