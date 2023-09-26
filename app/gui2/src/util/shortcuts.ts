import {
  CTRL,
  MouseButton,
  ShortcutRegistry,
  keybind,
  mousebind,
} from 'enso-authentication/src/dashboard/shortcuts'

const MOUSE_ACTIONS = [
  'replace-nodes-selection',
  'add-to-nodes-selection',
  'remove-from-nodes-selection',
  'toggle-nodes-selection',
  'invert-nodes-selection',
] as const
export type GUIMouseAction = (typeof MOUSE_ACTIONS)[number]

const KEYBOARD_ACTIONS = ['select-all-nodes', 'deselect-all-nodes'] as const
export type GUIKeyboardAction = (typeof KEYBOARD_ACTIONS)[number]

declare module 'enso-authentication/src/dashboard/shortcuts' {
  // Merge newly defined actions with existing interfaces.
  interface MouseActions extends Record<GUIMouseAction, true> {}
  interface KeyboardActions extends Record<GUIKeyboardAction, true> {}
}

export const shortcutRegistry = ShortcutRegistry.createWithDefaults()

shortcutRegistry.registerNewKeyboardActions(KEYBOARD_ACTIONS, {
  'select-all-nodes': [keybind('select-all-nodes', [CTRL], 'A')],
  'deselect-all-nodes': [keybind('deselect-all-nodes', [], 'Escape')],
})

shortcutRegistry.registerNewMouseActions(MOUSE_ACTIONS, {
  'replace-nodes-selection': [
    mousebind('replace-nodes-selection', [], MouseButton.move, 1),
    mousebind('replace-nodes-selection', [], MouseButton.left, 1),
  ],
  'add-to-nodes-selection': [
    mousebind('add-to-nodes-selection', ['Shift', CTRL], MouseButton.move, 1),
    mousebind('add-to-nodes-selection', ['Shift', CTRL], MouseButton.left, 1),
  ],
  'remove-from-nodes-selection': [
    mousebind('remove-from-nodes-selection', ['Shift', 'Alt'], MouseButton.move, 1),
    mousebind('add-to-nodes-selection', ['Shift', CTRL], MouseButton.left, 1),
  ],
  'toggle-nodes-selection': [
    mousebind('toggle-nodes-selection', ['Shift'], MouseButton.move, 1),
    mousebind('toggle-nodes-selection', ['Shift'], MouseButton.left, 1),
  ],
  'invert-nodes-selection': [
    mousebind('invert-nodes-selection', ['Shift', CTRL, 'Alt'], MouseButton.move, 1),
    mousebind('invert-nodes-selection', ['Shift', CTRL, 'Alt'], MouseButton.left, 1),
  ],
})
