import { isMacLike } from '@/util/events'

/** All possible modifier keys. */
export type ModifierKey = keyof typeof RAW_MODIFIER_FLAG

// ======================
// === Modifier flags ===
// ======================

const RAW_MODIFIER_FLAG = {
  Ctrl: 1 << 0,
  Alt: 1 << 1,
  Shift: 1 << 2,
  Meta: 1 << 3,
}

const MODIFIER_FLAG: Record<Modifier, number> = {
  Mod: isMacLike ? RAW_MODIFIER_FLAG.Meta : RAW_MODIFIER_FLAG.Ctrl,
  Alt: RAW_MODIFIER_FLAG.Alt,
  Shift: RAW_MODIFIER_FLAG.Shift,
  Meta: RAW_MODIFIER_FLAG.Meta,
}

/** A number representing the unique combination of modifier flags. */
function modifierFlagsForModifiers(modifiers: Modifier[]): ModifierFlags {
  let result = 0
  for (const modifier of modifiers) {
    result |= MODIFIER_FLAG[modifier]
  }
  return result as ModifierFlags
}

/** Any event that contains modifier keys. {@link KeyboardEvent}s and {@link MouseEvent}s fall into
 * this category. */
interface EventWithModifiers {
  ctrlKey: boolean
  altKey: boolean
  shiftKey: boolean
  metaKey: boolean
}

/** A number representing the unique combination of modifier flags for an event.. */
function modifierFlagsForEvent(event: EventWithModifiers): ModifierFlags {
  return ((event.ctrlKey ? RAW_MODIFIER_FLAG.Ctrl : 0) |
    (event.altKey ? RAW_MODIFIER_FLAG.Alt : 0) |
    (event.shiftKey ? RAW_MODIFIER_FLAG.Shift : 0) |
    (event.metaKey ? RAW_MODIFIER_FLAG.Meta : 0)) as ModifierFlags
}

/** These values MUST match the flags on `MouseEvent#button`.
 * See https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons */
const POINTER_BUTTON_FLAG = {
  PointerMain: 1 << 0,
  PointerSecondary: 1 << 1,
  PointerAux: 1 << 2,
  PointerBack: 1 << 3,
  PointerForward: 1 << 4,
} satisfies Record<Pointer, number> as Record<Pointer, PointerButtonFlags>

// ==========================
// === Autocomplete types ===
// ==========================

const allModifiers = ['Mod', 'Alt', 'Shift', 'Meta'] as const
type Modifier = (typeof allModifiers)[number]
type ModifierPlus = `${Modifier}+`
type LowercaseModifier = Lowercase<Modifier>
const allPointers = [
  'PointerMain',
  'PointerSecondary',
  'PointerAux',
  'PointerBack',
  'PointerForward',
] as const
type Pointer = (typeof allPointers)[number]
type LowercasePointer = Lowercase<Pointer>
/** This list is non-exhaustive. It is intentionally  */
const allKeys = [
  'Escape',
  'Enter',
  'Backspace',
  'Delete',
  // The key labeled as `Delete` - `Backspace` on macOS, `Delete` on all other platforms.
  'OsDelete',
  'Tab',
  'ArrowUp',
  'ArrowDown',
  'ArrowLeft',
  'ArrowRight',
  'Home',
  'End',
  'PageUp',
  'PageDown',
  'Insert',
  'Space',
  'A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G',
  'H',
  'I',
  'J',
  'K',
  'L',
  'M',
  'N',
  'O',
  'P',
  'Q',
  'R',
  'S',
  'T',
  'U',
  'V',
  'W',
  'X',
  'Y',
  'Z',
  '0',
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  '`',
  '-',
  '=',
  '~',
  '!',
  '@',
  '#',
  '$',
  '%',
  '^',
  '&',
  '*',
  '(',
  ')',
  '_',
  '+',
  '[',
  ']',
  '\\',
  '{',
  '}',
  '|',
  ';',
  "'",
  ':',
  '"',
  ',',
  '.',
  '/',
  '<',
  '>',
  '?',
  'F1',
  'F2',
  'F3',
  'F4',
  'F5',
  'F6',
  'F7',
  'F8',
  'F9',
  'F10',
  'F11',
  'F12',
] as const
type Key = (typeof allKeys)[number]
type LowercaseKey = Lowercase<Key>
type KeybindSegment = Modifier | Pointer | Key
const normalizedKeyboardSegmentLookup = Object.fromEntries<string>(
  [...allModifiers, ...allPointers, ...allKeys].map((entry) => [entry.toLowerCase(), entry]),
)
normalizedKeyboardSegmentLookup[''] = '+'
normalizedKeyboardSegmentLookup['space'] = ' '
normalizedKeyboardSegmentLookup['osdelete'] = isMacLike ? 'Delete' : 'Backspace'
type NormalizeKeybindSegment = {
  [K in KeybindSegment as Lowercase<K>]: K
}
type SuggestedKeybindSegment = ModifierPlus | Pointer | Key
type AutocompleteKeybind<T extends string, Key extends string = never> = T extends '+'
  ? T
  : T extends `${infer First}+${infer Rest}`
  ? Lowercase<First> extends LowercaseModifier
    ? `${NormalizeKeybindSegment[Lowercase<First>] & string}+${AutocompleteKeybind<Rest>}`
    : Lowercase<First> extends LowercasePointer | LowercaseKey
    ? AutocompleteKeybind<Rest, NormalizeKeybindSegment[Lowercase<First>] & string>
    : `${Modifier}+${AutocompleteKeybind<Rest>}`
  : T extends ''
  ? SuggestedKeybindSegment
  : Lowercase<T> extends LowercasePointer | LowercaseKey
  ? NormalizeKeybindSegment[Lowercase<T>]
  : Lowercase<T> extends LowercaseModifier
  ? [Key] extends [never]
    ? `${NormalizeKeybindSegment[Lowercase<T>] & string}+${SuggestedKeybindSegment}`
    : `${NormalizeKeybindSegment[Lowercase<T>] & string}+${Key}`
  : [Key] extends [never]
  ? SuggestedKeybindSegment
  : Key

type AutocompleteKeybinds<T extends string[]> = {
  [K in keyof T]: AutocompleteKeybind<T[K]>
}

// `never extends T ? Result : InferenceSource` is a trick to unify `T` with the actual type of the
// argument.
type Keybinds<T extends Record<K, string[]>, K extends keyof T = keyof T> = never extends T
  ? {
      [K in keyof T]: AutocompleteKeybinds<T[K]>
    }
  : T

declare const brandKey: unique symbol
type Key_ = string & { [brandKey]: true }
declare const brandModifierFlags: unique symbol
type ModifierFlags = number & { [brandModifierFlags]: true }
declare const brandPointerButtonFlags: unique symbol
type PointerButtonFlags = number & { [brandPointerButtonFlags]: true }

const definedNamespaces = new Set<string>()

export const DefaultHandler = Symbol('default handler')

/**
 * Define key bindings for given namespace.
 *
 * This function takes list of actions with default bindings, and returns an object which allows
 * making event handler which in turn may be added as an appropriate event listener. It may handle
 * both keyboard and mouse events.
 *
 * The event handler assigns functions to the corresponding action. The function may return false
 * if the event should be considered not handled (and thus propagated). Returning true or just
 * nothing from the function will cause propagation of event stop.
 *
 * @param namespace should be unique among other `defineKebinds` calls.
 * @param bindings is an object defining actions and their key bindings. Each property name is an
 * action name, and value is list of default key bindings. See "Keybinds should be parsed
 * correctly" test for examples of valid strings.
 * @returns an object with defined `handler` function.
 *
 * Example:
 *
 * Define bindings:
 * ```
 * const graphBindings = defineKeybinds('graph-editor', {
 *   undo: ['Mod+Z'],
 *   redo: ['Mod+Y', 'Mod+Shift+Z'],
 *   dragScene: ['PointerAux', 'Mod+PointerMain'],
 *   openComponentBrowser: ['Enter'],
 *   newNode: ['N'],
 * })
 * ```
 *
 * Then make a handler:
 * ```
 * const graphBindingsHandler = graphBindings.handler({
 *   undo() {
 *     projectStore.module?.undoManager.undo()
 *   },
 *   redo() {
 *     projectStore.module?.undoManager.redo()
 *   },
 *   openComponentBrowser() {
 *     if (keyboardBusy()) return false
 *     if (navigator.sceneMousePos != null && !componentBrowserVisible.value) {
 *       componentBrowserPosition.value = navigator.sceneMousePos
 *       componentBrowserVisible.value = true
 *     }
 *   },
 *   newNode() {
 *     if (keyboardBusy()) return false
 *     if (navigator.sceneMousePos != null) {
 *       graphStore.createNode(navigator.sceneMousePos, 'hello "world"! 123 + x')
 *     }
 *   },
 * })
 * ```
 *
 * And then pass the handler to the event listener:
 * ```
 * useEvent(window, 'keydown', graphBindingsHandler)
 * ```
 */
export function defineKeybinds<
  T extends Record<BindingName, [] | string[]>,
  BindingName extends keyof T = keyof T,
>(namespace: string, bindings: Keybinds<T>) {
  if (definedNamespaces.has(namespace)) {
    console.warn(`The keybind namespace '${namespace}' has already been defined.`)
  } else {
    definedNamespaces.add(namespace)
  }
  const keyboardShortcuts: Partial<Record<Key_, Record<ModifierFlags, Set<BindingName>>>> = {}
  const mouseShortcuts: Record<PointerButtonFlags, Record<ModifierFlags, Set<BindingName>>> = []

  for (const [name_, keybindStrings] of Object.entries(bindings)) {
    const name = name_ as BindingName
    for (const keybindString of keybindStrings as string[]) {
      const keybind = parseKeybindString(keybindString)
      switch (keybind.type) {
        case 'keybind': {
          const shortcutsByKey = (keyboardShortcuts[keybind.key] ??= [])
          const shortcutsByModifier = (shortcutsByKey[keybind.modifierFlags] ??= new Set())
          shortcutsByModifier.add(name)
          break
        }
        case 'mousebind': {
          const shortcutsByKey = (mouseShortcuts[keybind.key] ??= [])
          const shortcutsByModifier = (shortcutsByKey[keybind.modifierFlags] ??= new Set())
          shortcutsByModifier.add(name)
          break
        }
      }
    }
  }

  function handler<Event_ extends KeyboardEvent | MouseEvent | PointerEvent>(
    handlers: Partial<
      Record<BindingName | typeof DefaultHandler, (event: Event_) => boolean | void>
    >,
  ): (event: Event_, stopAndPrevent?: boolean) => boolean {
    return (event, stopAndPrevent = true) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const keybinds =
        event instanceof KeyboardEvent
          ? keyboardShortcuts[event.key.toLowerCase() as Key_]?.[eventModifierFlags]
          : mouseShortcuts[event.buttons as PointerButtonFlags]?.[eventModifierFlags]
      let handle = handlers[DefaultHandler]
      if (keybinds != null) {
        for (const bindingName in handlers) {
          if (keybinds?.has(bindingName as BindingName)) {
            handle = handlers[bindingName as BindingName]
            break
          }
        }
      }
      if (handle == null) {
        return false
      }
      if (handle(event) === false) {
        return false
      }
      if (stopAndPrevent) {
        event.stopImmediatePropagation()
        event.preventDefault()
      }
      return true
    }
  }

  return { handler }
}

/** A type predicate that narrows the potential child of the array. */
function includesPredicate<T extends U, U>(array: readonly T[]) {
  const array_: readonly unknown[] = array
  return (element: unknown): element is T => array_.includes(element)
}

const isModifier = includesPredicate(allModifiers)
const isPointer = includesPredicate(allPointers)

function decomposeKeybindString(string: string): ModifierStringDecomposition {
  const parts = string
    .trim()
    .split(/[\s+]+/)
    .map((part) => normalizedKeyboardSegmentLookup[part.toLowerCase()] ?? part)
  const modifiers = parts.filter(isModifier)
  const key = parts.find((part) => !isModifier(part))
  return {
    key: key ?? '',
    modifiers,
  }
}

function parseKeybindString(string: string): Keybind | Mousebind {
  const decomposed = decomposeKeybindString(string)
  if (isPointer(decomposed.key)) {
    return {
      type: 'mousebind',
      key: POINTER_BUTTON_FLAG[decomposed.key],
      modifierFlags: modifierFlagsForModifiers(decomposed.modifiers),
    }
  } else {
    return {
      type: 'keybind',
      key: decomposed.key.toLowerCase() as Key_,
      modifierFlags: modifierFlagsForModifiers(decomposed.modifiers),
    }
  }
}

interface ModifierStringDecomposition {
  key: string
  modifiers: Modifier[]
}

interface Keybind {
  type: 'keybind'
  key: Key_
  modifierFlags: ModifierFlags
}

interface Mousebind {
  type: 'mousebind'
  key: PointerButtonFlags
  modifierFlags: ModifierFlags
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
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
}
