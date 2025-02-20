import {
  decomposeKeybindString,
  defineKeybinds,
  normalizedKeyboardSegmentLookup,
} from '@/util/shortcuts'
import { beforeAll, expect, test, vi, type MockInstance } from 'vitest'

// See https://github.com/thymikee/jest-preset-angular/issues/245#issuecomment-576296325
class MockPointerEvent extends MouseEvent {}
beforeAll(() => {
  ;(window as any).PointerEvent = MockPointerEvent
})

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

const testBindings = defineKeybinds('testBindings', {
  mouse: ['PointerMain'],
  mouseWithModifier: ['Mod+PointerMain'],
  mouseWithAllModifiers: ['Shift+Alt+Mod+Meta+PointerMain'],
  a: ['A'],
  b: ['B'],
  anotherA: ['A'],
  aWithModifier: ['Shift+A'],
})

test.each([
  {
    event: new MouseEvent('click'),
    handlers: ['mouse', 'mouseWithModifier', 'mouseWithAllModifiers'],
    expected: ['mouse'],
  },
  {
    event: new MouseEvent('click'),
    passingHandlers: ['mouse', 'mouseWithModifier', 'mouseWithAllModifiers'],
    expected: ['mouse'],
    expectPropagation: true,
  },
  {
    event: new MouseEvent('click', { ctrlKey: true }),
    handlers: ['mouse', 'mouseWithModifier', 'mouseWithAllModifiers'],
    expected: ['mouseWithModifier'],
  },
  {
    event: new MouseEvent('click', { altKey: true }),
    handlers: ['mouse', 'mouseWithModifier', 'mouseWithAllModifiers'],
    expected: [],
    expectPropagation: true,
  },
  {
    event: new MouseEvent('click', { altKey: true, ctrlKey: true, metaKey: true, shiftKey: true }),
    handlers: ['mouse', 'mouseWithModifier', 'mouseWithAllModifiers'],
    expected: ['mouseWithAllModifiers'],
  },
  {
    event: new KeyboardEvent('keypress', { key: 'A' }),
    handlers: ['a', 'b', 'anotherA', 'aWithModifier'],
    expected: ['a'],
  },
  {
    event: new KeyboardEvent('keypress', { key: 'B' }),
    handlers: ['a', 'b', 'anotherA', 'aWithModifier'],
    expected: ['b'],
  },
  {
    event: new KeyboardEvent('keypress', { key: 'A', shiftKey: true }),
    handlers: ['a', 'b', 'anotherA', 'aWithModifier'],
    expected: ['aWithModifier'],
  },
  {
    event: new KeyboardEvent('keypress', { key: 'A' }),
    handlers: ['b', 'aWithModifier'],
    passingHandlers: ['a', 'anotherA'],
    expected: ['a', 'anotherA'],
    expectPropagation: true,
  },
  {
    event: new KeyboardEvent('keypress', { key: 'A' }),
    handlers: ['anotherA', 'b', 'aWithModifier'],
    expected: ['anotherA'],
  },
])(
  'Event $event is handled by $expected (propagation expected: $expectPropagation)',
  ({ event, expected, expectPropagation, handlers, passingHandlers }) => {
    const definedHandlers: Record<string, MockInstance> = {}
    for (const handler of handlers ?? []) definedHandlers[handler] = vi.fn()
    for (const handler of passingHandlers ?? []) definedHandlers[handler] = vi.fn(() => false)
    const preventDefault = vi.spyOn(event, 'preventDefault')
    const stopImmediatePropagation = vi.spyOn(event, 'stopImmediatePropagation')

    const handler = testBindings.handler(definedHandlers)
    const handled = handler(event)

    const expectedSet = new Set(expected)
    for (const handler in definedHandlers) {
      if (expectedSet.has(handler)) expect(definedHandlers[handler], handler).toHaveBeenCalledOnce()
      else expect(definedHandlers[handler], handler).not.toHaveBeenCalled()
    }
    expect(handled).toBe(expectPropagation !== true)
    if (expectPropagation) {
      expect(preventDefault).not.toHaveBeenCalled()
      expect(stopImmediatePropagation).not.toHaveBeenCalled()
    } else {
      expect(preventDefault).toHaveBeenCalled()
      expect(stopImmediatePropagation).toHaveBeenCalled()
    }
  },
)
