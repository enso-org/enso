import { isMacLike } from './events'

// This file MUST be a `.tsx` file so that Tailwind includes the CSS classes used here.

/* eslint-disable @typescript-eslint/naming-convention */

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
  ' ',
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
const normalizedKeyboardSegmentLookup = Object.fromEntries<KeybindSegment | undefined>(
  [...allModifiers, ...allPointers, ...allKeys].map((entry) => [entry.toLowerCase(), entry]),
)
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

export function defineKeybinds<
  T extends Record<BindingName, [] | string[]>,
  BindingName extends keyof T = keyof T,
>(namespace: string, bindings: Keybinds<T>) {
  if (definedNamespaces.has(namespace)) {
    console.error(`The keybind namespace '${namespace}' has already been defined.`)
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

  function keyboardHandler(
    handlers: Partial<Record<BindingName | typeof DefaultHandler, (event: KeyboardEvent) => void>>,
    stopImmediatePropagationOnMatch = true,
    preventDefaultOnMatch = true,
  ): (event: KeyboardEvent) => boolean {
    return (event) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const keybinds = keyboardShortcuts[event.key.toLowerCase() as Key_]?.[eventModifierFlags]
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
      if (stopImmediatePropagationOnMatch) {
        event.stopImmediatePropagation()
      }
      if (preventDefaultOnMatch) {
        event.preventDefault()
      }
      handle(event)
      return true
    }
  }

  function mouseHandler(
    handlers: Partial<Record<BindingName | typeof DefaultHandler, (event: MouseEvent) => void>>,
    stopImmediatePropagationOnMatch = true,
    preventDefaultOnMatch = true,
  ): (event: MouseEvent) => boolean {
    return (event) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const keybinds = mouseShortcuts[event.buttons as PointerButtonFlags]?.[eventModifierFlags]
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
      if (stopImmediatePropagationOnMatch) {
        event.stopImmediatePropagation()
      }
      if (preventDefaultOnMatch) {
        event.preventDefault()
      }
      handle(event)
      return true
    }
  }

  return { keyboardHandler, mouseHandler }
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
    .map((part) => normalizedKeyboardSegmentLookup[part] ?? part)
  const modifiers = parts.filter(isModifier)
  const key = parts.find((part) => !isModifier(part))
  return {
    // If the key is blank, assume it was a `+` that was removed by the splitting regex.
    key: key === '' ? '+' : key ?? '',
    modifiers,
  }
}

function parseKeybindString(string: string): Keybind | Mousebind {
  const decomposed = decomposeKeybindString(string)
  const normalized =
    decomposed.modifiers.length === 0
      ? decomposed.key
      : `${decomposed.modifiers.join('+')}+${decomposed.key}`
  if (normalized !== string) {
    console.warn(`Modifier string '${string}' should be '${normalized}'`)
  }
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

// =================
// === Constants ===
// =================

// FIXME [sb]: This should be present in the keybinds too, just like `Mod`.
/** The key known as the `Delete` key for the current platform. */
const delete_ = isMacLike ? 'Backspace' : 'Delete'
