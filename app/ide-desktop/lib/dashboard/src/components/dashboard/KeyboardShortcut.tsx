/** @file A visual representation of a keyboard shortcut. */
import * as React from 'react'

import CommandKeyIcon from 'enso-assets/command_key.svg'
import CtrlKeyIcon from 'enso-assets/ctrl_key.svg'
import OptionKeyIcon from 'enso-assets/option_key.svg'
import ShiftKeyIcon from 'enso-assets/shift_key.svg'
import WindowsKeyIcon from 'enso-assets/windows_key.svg'
import * as detect from 'enso-common/src/detect'

import type * as dashboardInputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import SvgMask from '#/components/SvgMask'

import * as inputBindingsModule from '#/utilities/inputBindings'

// ========================
// === KeyboardShortcut ===
// ========================

/** The size (both width and height) of key icons. */
const ICON_SIZE_PX = 13

const ICON_STYLE = { width: ICON_SIZE_PX, height: ICON_SIZE_PX }

/** Icons for modifier keys (if they exist). */
const MODIFIER_MAPPINGS: Readonly<
  Record<detect.Platform, Partial<Record<inputBindingsModule.ModifierKey, React.ReactNode>>>
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
      <span key="Meta" className="text">
        Super
      </span>
    ),
  },
  [detect.Platform.unknown]: {
    // Assume the system is Unix-like and calls the key that triggers `event.metaKey`
    // the "Super" key.
    Meta: (
      <span key="Meta" className="text">
        Super
      </span>
    ),
  },
  /* eslint-enable @typescript-eslint/naming-convention */
}

/** Props for a {@link KeyboardShortcut}, specifying the keyboard action. */
export interface KeyboardShortcutActionProps {
  readonly action: dashboardInputBindings.DashboardBindingKey
}

/** Props for a {@link KeyboardShortcut}, specifying the shortcut string. */
export interface KeyboardShortcutShortcutProps {
  readonly shortcut: string
}

/** Props for a {@link KeyboardShortcut}. */
export type KeyboardShortcutProps = KeyboardShortcutActionProps | KeyboardShortcutShortcutProps

/** A visual representation of a keyboard shortcut. */
export default function KeyboardShortcut(props: KeyboardShortcutProps) {
  const inputBindings = inputBindingsProvider.useInputBindings()
  const shortcutString =
    'shortcut' in props ? props.shortcut : inputBindings.metadata[props.action].bindings[0]
  if (shortcutString == null) {
    return null
  } else {
    const shortcut = inputBindingsModule.decomposeKeybindString(shortcutString)
    const modifiers = [...shortcut.modifiers]
      .sort(inputBindingsModule.compareModifiers)
      .map(inputBindingsModule.toModifierKey)
    return (
      <div
        className={`flex items-center h-text ${
          detect.isOnMacOS() ? 'gap-modifiers-macos' : 'gap-modifiers'
        }`}
      >
        {modifiers.map(
          modifier =>
            MODIFIER_MAPPINGS[detect.platform()][modifier] ?? (
              <span key={modifier} className="text">
                {modifier}
              </span>
            )
        )}
        <span className="text">{shortcut.key === ' ' ? 'Space' : shortcut.key}</span>
      </div>
    )
  }
}
