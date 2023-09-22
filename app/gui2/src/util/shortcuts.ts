import { ShortcutRegistry } from 'enso-authentication/src/dashboard/shortcuts'

const MOUSE_ACTIONS = [
  'clear-nodes-selection',
  'replace-nodes-selection',
  'add-to-nodes-selection',
  'remove-from-nodes-selection',
  'invert-nodes-selection',
] as const
type GUIMouseAction = (typeof MOUSE_ACTIONS)[number]

const KEYBOARD_ACTIONS = [] as const
type GUIKeyboardAction = (typeof KEYBOARD_ACTIONS)[number]

declare module 'enso-authentication/src/dashboard/shortcuts' {
  // Merge newly defined actions with existing interfaces.
  interface MouseActions extends Record<GUIMouseAction, true> {}
  interface KeyboardActions extends Record<GUIKeyboardAction, true> {}
}

export const shortcutRegistry = ShortcutRegistry.createWithDefaults()
// TODO [sb]: figure out what else to export
