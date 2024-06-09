/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import * as detect from 'enso-common/src/detect'

import * as shortcutsModule from '#/utilities/inputBindings'

// =============
// === Tests ===
// =============

// NOTE: macOS using the meta key has not been tested, as it is not possible to override
// `navigator.userAgent`.

const DELETE_KEY = detect.isOnMacOS() ? 'Backspace' : 'Delete'

v.test.each([
  { keybind: 'A', expected: { modifiers: [], key: 'A' } },
  { keybind: 'b', expected: { modifiers: [], key: 'B' } },
  { keybind: 'Space', expected: { modifiers: [], key: ' ' } },
  { keybind: 'Mod+Space', expected: { modifiers: ['Mod'], key: ' ' } },
  // `+`
  { keybind: 'Mod++', expected: { modifiers: ['Mod'], key: '+' } },
  // `+` and capitalization
  { keybind: 'mod++', expected: { modifiers: ['Mod'], key: '+' } },
  {
    keybind: 'Mod+Alt+Shift+PointerMain',
    expected: { modifiers: ['Mod', 'Alt', 'Shift'], key: 'PointerMain' },
  },
  // Misspelling. Assume it is an unknown key
  {
    keybind: 'shift+Alt+Mode+PointerMain',
    expected: { modifiers: ['Shift', 'Alt'], key: 'Mode' },
  },
  // Capitalization
  {
    keybind: 'meta+shift+alt+mod+pointermain',
    expected: { modifiers: ['Meta', 'Shift', 'Alt', 'Mod'], key: 'PointerMain' },
  },
  // Repeated keys
  {
    keybind: 'A+A+A+A+A+A+A+A+A+A+A+A+A+A',
    expected: { modifiers: [], key: 'A' },
  },
  {
    keybind: 'mod++++++++++++++++',
    expected: { modifiers: ['Mod'], key: '+' },
  },
  // Misc tests
  {
    keybind: 'osDEleTE',
    // The specific key is OS-dependent. Look it up in the same lookup used by
    // `decomposeKeybindString` so that it is guaranteed to be the right key on all systems.
    expected: { modifiers: [], key: DELETE_KEY },
  },
])('Keybinds should be parsed correctly', ({ keybind, expected }) => {
  const decomposed = shortcutsModule.decomposeKeybindString(keybind)
  v.expect(decomposed).toEqual(expected)
})
