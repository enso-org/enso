import {
  CTRL,
  MouseButton,
  ShortcutRegistry,
  keybind,
  mousebind,
} from 'enso-authentication/src/dashboard/shortcuts'
import { useDocumentEvent } from './events'

const MOUSE_ACTIONS = [
  'clear-nodes-selection',
  'replace-nodes-selection',
  'add-to-nodes-selection',
  'remove-from-nodes-selection',
  'toggle-nodes-selection',
  'invert-nodes-selection',
] as const
type GUIMouseAction = (typeof MOUSE_ACTIONS)[number]

const KEYBOARD_ACTIONS = ['select-all-nodes', 'deselect-all-nodes'] as const
type GUIKeyboardAction = (typeof KEYBOARD_ACTIONS)[number]

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
  'clear-nodes-selection': [mousebind('clear-nodes-selection', [], MouseButton.left, 1)],
  'replace-nodes-selection': [mousebind('replace-nodes-selection', [], MouseButton.left, 1)],
  'add-to-nodes-selection': [
    mousebind('add-to-nodes-selection', ['Shift', CTRL], MouseButton.left, 1),
  ],
  'remove-from-nodes-selection': [
    mousebind('remove-from-nodes-selection', ['Shift', 'Alt'], MouseButton.left, 1),
  ],
  'toggle-nodes-selection': [mousebind('toggle-nodes-selection', ['Shift'], MouseButton.left, 1)],
  'invert-nodes-selection': [
    mousebind('invert-nodes-selection', ['Shift', CTRL, 'Alt'], MouseButton.left, 1),
  ],
})

// useDocumentEvent('keydown', shortcutRegistry.handleKeyboardEvent.bind(shortcutRegistry))
