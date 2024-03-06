import { decomposeKeybindString, normalizedKeyboardSegmentLookup } from '@/util/shortcuts'
import { expect, test } from 'vitest'

test.each([
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
    // The specific key is OS-dependent. Look it up in the samee lookup used by
    // `decomposeKeybindString` so that it is guaranteed to be the right key on all systems.
    expected: { modifiers: [], key: normalizedKeyboardSegmentLookup['osdelete'] },
  },
])('Keybinds should be parsed correctly', ({ keybind, expected }) => {
  const decomposed = decomposeKeybindString(keybind)
  expect(decomposed).toEqual(expected)
})
