/** @file Exports `defineKeybinds`, a function to define a namespace containing keyboard and mouse
 * shortcuts. */
import * as detect from 'enso-common/src/detect'

import * as newtype from '#/utilities/newtype'

// ================
// === Newtypes ===
// ================

/** A keyboard key obtained from `KeyboardEvent.key`. */
type KeyName = newtype.Newtype<string, 'keyboard key'>
// eslint-disable-next-line @typescript-eslint/no-redeclare, @typescript-eslint/naming-convention
const KeyName = newtype.newtypeConstructor<KeyName>()
/** A bitset of flags representing each keyboard modifier key. */
type ModifierFlags = newtype.Newtype<number, 'modifier flags'>
// eslint-disable-next-line @typescript-eslint/no-redeclare
const ModifierFlags = newtype.newtypeConstructor<ModifierFlags>()
/** A bitset of flags representing each mouse pointer. */
type PointerButtonFlags = newtype.Newtype<number, 'pointer button flags'>
// eslint-disable-next-line @typescript-eslint/no-redeclare
const PointerButtonFlags = newtype.newtypeConstructor<PointerButtonFlags>()

// =============
// === Types ===
// =============

/** All possible modifier keys. */
export type ModifierKey = keyof typeof RAW_MODIFIER_FLAG

/** The target of a {@link KeyboardEvent}, {@link MouseEvent}, or {@link PointerEvent}. */
export interface InputEventTarget<
  EventName extends string,
  Event extends KeyboardEvent | MouseEvent | PointerEvent,
> {
  readonly addEventListener: (eventName: EventName, handler: (event: Event) => void) => void
  readonly removeEventListener: (eventName: EventName, handler: (event: Event) => void) => void
}

/** An intermediate representation of a keybind, in which all segments have been tokenized but
 * before converting into either a {@link Keybind} or a {@link Mousebind}. */
interface ModifierStringDecomposition {
  readonly key: string
  readonly modifiers: Modifier[]
}

/** A keyboard shortcut. */
export interface Keybind {
  readonly type: 'keybind'
  readonly key: KeyName
  readonly modifierFlags: ModifierFlags
}

/** A mouse shortcut. */
export interface Mousebind {
  readonly type: 'mousebind'
  readonly key: PointerButtonFlags
  readonly modifierFlags: ModifierFlags
}

// ======================
// === Modifier flags ===
// ======================

/* eslint-disable @typescript-eslint/naming-convention */
const RAW_MODIFIER_FLAG = {
  Ctrl: 1 << 0,
  Alt: 1 << 1,
  Shift: 1 << 2,
  Meta: 1 << 3,
}

const MODIFIER_FLAG: Record<Modifier, number> = {
  Mod: detect.isOnMacOS() ? RAW_MODIFIER_FLAG.Meta : RAW_MODIFIER_FLAG.Ctrl,
  Alt: RAW_MODIFIER_FLAG.Alt,
  Shift: RAW_MODIFIER_FLAG.Shift,
  Meta: RAW_MODIFIER_FLAG.Meta,
}
/* eslint-enable @typescript-eslint/naming-convention */

/** A number representing the unique combination of modifier flags. */
function modifierFlagsForModifiers(modifiers: Modifier[]): ModifierFlags {
  let result = 0
  for (const modifier of modifiers) {
    result |= MODIFIER_FLAG[modifier]
  }
  return ModifierFlags(result)
}

/** Any event that contains modifier keys. {@link KeyboardEvent}s and {@link MouseEvent}s fall into
 * this category. */
interface EventWithModifiers {
  readonly ctrlKey: boolean
  readonly altKey: boolean
  readonly shiftKey: boolean
  readonly metaKey: boolean
}

/** A number representing the unique combination of modifier flags for an event.. */
function modifierFlagsForEvent(event: EventWithModifiers): ModifierFlags {
  // eslint-disable-next-line no-restricted-syntax
  return ModifierFlags(
    (event.ctrlKey ? RAW_MODIFIER_FLAG.Ctrl : 0) |
      (event.altKey ? RAW_MODIFIER_FLAG.Alt : 0) |
      (event.shiftKey ? RAW_MODIFIER_FLAG.Shift : 0) |
      (event.metaKey ? RAW_MODIFIER_FLAG.Meta : 0)
  )
}

/* eslint-disable @typescript-eslint/naming-convention */
/** These values MUST match the flags on `MouseEvent#button`.
 * See https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons */
const POINTER_BUTTON_FLAG: Readonly<Record<Pointer, PointerButtonFlags>> = {
  PointerMain: PointerButtonFlags(1 << 0),
  PointerSecondary: PointerButtonFlags(1 << 1),
  PointerAux: PointerButtonFlags(1 << 2),
  PointerBack: PointerButtonFlags(1 << 3),
  PointerForward: PointerButtonFlags(1 << 4),
}
/* eslint-enable @typescript-eslint/naming-convention */

// ==========================
// === Autocomplete types ===
// ==========================

const ALL_MODIFIERS = ['Mod', 'Alt', 'Shift', 'Meta'] as const
/** All valid keyboard modifier keys. */
type Modifier = (typeof ALL_MODIFIERS)[number]
/** All valid keyboard modifier keys, normalized to lowercase for autocomplete purposes. */
type LowercaseModifier = Lowercase<Modifier>
const ALL_POINTERS = [
  'PointerMain',
  'PointerSecondary',
  'PointerAux',
  'PointerBack',
  'PointerForward',
] as const
/** All valid mouse pointer buttons. */
type Pointer = (typeof ALL_POINTERS)[number]
/** All valid mouse pointer buttons, normalized to lowercase for autocomplete purposes. */
type LowercasePointer = Lowercase<Pointer>
/** This list is non-exhaustive. It is intentionally limited to keys found on most keyboards. */
const ALL_KEYS = [
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
/** Common keyboard keys. */
type Key = (typeof ALL_KEYS)[number]
/** Common keyboard keys, normalized to lowercase for autocomplete purposes. */
type LowercaseKey = Lowercase<Key>
/** A segment of a keyboard shortcut. */
type KeybindSegment = Key | Modifier | Pointer
const normalizedKeyboardSegmentLookup = Object.fromEntries<string>(
  [...ALL_MODIFIERS, ...ALL_POINTERS, ...ALL_KEYS].map(entry => [entry.toLowerCase(), entry])
)
normalizedKeyboardSegmentLookup[''] = '+'
normalizedKeyboardSegmentLookup['space'] = ' '
normalizedKeyboardSegmentLookup['osdelete'] = detect.isOnMacOS() ? 'Backspace' : 'Delete'
/** A mapping between the lowercased segment of a keyboard shortcut to its properly capitalized
 * normalized form. */
type NormalizeKeybindSegment = {
  [K in KeybindSegment as Lowercase<K>]: K
}
/** A segment suggestible by autocomplete. */
type SuggestedKeybindSegment = Key | Pointer | `${Modifier}+`
/** A helper type used to autocomplete and validate a single keyboard shortcut in the editor. */
type AutocompleteKeybind<T extends string, FoundKeyName extends string = never> = T extends '+'
  ? T
  : T extends `${infer First}+${infer Rest}`
  ? Lowercase<First> extends LowercaseModifier
    ? `${NormalizeKeybindSegment[Lowercase<First>] & string}+${AutocompleteKeybind<Rest>}`
    : Lowercase<First> extends LowercaseKey | LowercasePointer
    ? AutocompleteKeybind<Rest, NormalizeKeybindSegment[Lowercase<First>] & string>
    : `${Modifier}+${AutocompleteKeybind<Rest>}`
  : T extends ''
  ? SuggestedKeybindSegment
  : Lowercase<T> extends LowercaseKey | LowercasePointer
  ? NormalizeKeybindSegment[Lowercase<T>]
  : Lowercase<T> extends LowercaseModifier
  ? [FoundKeyName] extends [never]
    ? `${NormalizeKeybindSegment[Lowercase<T>] & string}+${SuggestedKeybindSegment}`
    : `${NormalizeKeybindSegment[Lowercase<T>] & string}+${FoundKeyName}`
  : [FoundKeyName] extends [never]
  ? SuggestedKeybindSegment
  : FoundKeyName

/** A helper type used to autocomplete and validate an array of keyboard shortcuts in the editor.
 */
type AutocompleteKeybinds<T extends string[]> = {
  [K in keyof T]: AutocompleteKeybind<T[K]>
}

/** A helper type used to autocomplete and validate an object containing actions and their
 * corresponding keyboard shortcuts. */
// `never extends T ? Result : InferenceSource` is a trick to unify `T` with the actual type of the
// argument.
type Keybinds<T extends Record<keyof T, string[]>> = never extends T
  ? {
      [K in keyof T]: AutocompleteKeybinds<T[K]>
    }
  : T

const DEFINED_NAMESPACES = new Set<string>()

export const DEFAULT_HANDLER = Symbol('default handler')

/** Define key bindings for the given namespace.
 *
 * This function takes list of actions with default bindings, and returns an object which allows
 * making event handler which in turn may be added as an appropriate event listener. It may handle
 * both keyboard and mouse events.
 *
 * The event handler assigns functions to the corresponding action. The function may return false
 * if the event should be considered not handled (and thus propagated). Returning true or just
 * nothing from the function will cause propagation of event stop.
 * @param namespace - should be unique among other `defineKeybinds` calls.
 * @param bindings - an object defining actions and their key bindings. Each property name is an
 * action name, and value is list of default key bindings. See "Keybinds should be parsed
 * correctly" test for examples of valid strings.
 * @returns an object with defined `handler` function.
 * @example
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
export function defineBindingNamespace<T extends Record<keyof T, string[] | []>>(
  namespace: string,
  bindings: Keybinds<T>
) {
  /** The name of a binding in this set of keybinds. */
  type BindingKey = keyof T
  if (DEFINED_NAMESPACES.has(namespace)) {
    // eslint-disable-next-line no-restricted-properties
    console.warn(`The keybind namespace '${namespace}' has already been defined.`)
  } else {
    DEFINED_NAMESPACES.add(namespace)
  }
  const keyboardShortcuts: Partial<
    Record<KeyName, Partial<Record<ModifierFlags, Set<BindingKey>>>>
  > = {}
  const mouseShortcuts: Partial<
    Record<PointerButtonFlags, Partial<Record<ModifierFlags, Set<BindingKey>>>>
  > = []

  // This is SAFE, as it is a `readonly` upcast.
  for (const [nameRaw, keybindStrings] of Object.entries(
    // eslint-disable-next-line no-restricted-syntax
    bindings as Readonly<Record<string, string[]>>
  )) {
    // This is SAFE, as `Keybinds<T>` is a type derived from `T`.
    // eslint-disable-next-line no-restricted-syntax
    const name = nameRaw as BindingKey
    for (const keybindString of keybindStrings) {
      const keybind = parseKeybindString(keybindString)
      switch (keybind.type) {
        case 'keybind': {
          const shortcutsByKey = (keyboardShortcuts[keybind.key] ??= [])
          // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
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

  const handler = <Event extends KeyboardEvent | MouseEvent | PointerEvent>(
    handlers: Partial<
      // This MUST be `void` to allow implicit returns.
      // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
      Record<BindingKey | typeof DEFAULT_HANDLER, (event: Event) => boolean | void>
    >
  ): ((event: Event, stopAndPrevent?: boolean) => boolean) => {
    return (event, stopAndPrevent = true) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const keybinds =
        event instanceof KeyboardEvent
          ? keyboardShortcuts[KeyName(event.key.toLowerCase())]?.[eventModifierFlags]
          : mouseShortcuts[PointerButtonFlags(event.buttons)]?.[eventModifierFlags]
      let handle = handlers[DEFAULT_HANDLER]
      if (keybinds != null) {
        for (const bindingNameRaw in handlers) {
          // This is SAFE, because `handlers` is an object with identical keys to `T`,
          // which `BindingName` is also derived from.
          // eslint-disable-next-line no-restricted-syntax
          const bindingName = bindingNameRaw as BindingKey
          if (keybinds.has(bindingName)) {
            handle = handlers[bindingName]
            break
          }
        }
      }
      if (handle == null) {
        return false
      } else if (handle(event) === false) {
        return false
      } else {
        if (stopAndPrevent) {
          event.stopImmediatePropagation()
          event.preventDefault()
        }
        return true
      }
    }
  }

  const attach = <
    EventName extends string,
    Event extends KeyboardEvent | MouseEvent | PointerEvent,
  >(
    target: InputEventTarget<EventName, Event>,
    eventName: EventName,
    handlers: Partial<
      // This MUST be `void` to allow implicit returns.
      // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
      Record<BindingKey | typeof DEFAULT_HANDLER, (event: Event) => boolean | void>
    >
  ) => {
    const newHandler = handler(handlers)
    target.addEventListener(eventName, newHandler)
    return () => {
      target.removeEventListener(eventName, newHandler)
    }
  }

  const reset = (bindingName: BindingKey) => {
    // TODO: reset to its original definition in `bindings`.
  }

  const deleteFunction = (bindingName: BindingKey, binding: Keybind | Mousebind) => {
    //
  }

  // It is an option to accept a string instead, but that only really works for keybinds that are
  // already known at compile time.
  const add = (bindingName: BindingKey, binding: Keybind | Mousebind) => {
    //
  }

  return {
    /** Return an event handler that handles a native keyboard, mouse or pointer event. */
    handler,
    /** Attach an event listener to an {@link EventTarget} and return a function to detach the
     * listener. */
    attach,
    /** Reset the entire list of bindings for a specific action to its default value. */
    reset,
    /** Delete one specific binding from the bindings for a specific action.
     * The binding MUST compare reference equal to be deleted. */
    delete: deleteFunction,
  } as const
}

/** A function to define a bindings object that can be passed to {@link defineBindingNamespace}.
 * Useful when wanting to create reusable keybind definitions, or non-global keybind definitions. */
export function defineBindings<T extends Record<keyof T, string[] | []>>(bindings: Keybinds<T>) {
  return bindings
}

/** A type predicate that narrows the potential child of the array. */
function includesPredicate<T extends U, U>(array: readonly T[]) {
  const wideArray: readonly unknown[] = array
  return (element: unknown): element is T => wideArray.includes(element)
}

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const isModifier = includesPredicate(ALL_MODIFIERS)
// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const isPointer = includesPredicate(ALL_POINTERS)

/** Convert a keybind string to an intermediate form containing both the key and its modifiers
 * (if any).
 *
 * Although this is exported, it should ONLY be used for testing, as it is an implementation
 * detail. */
export function decomposeKeybindString(string: string): ModifierStringDecomposition {
  const parts = string
    .trim()
    .split(/[\s+]+/)
    .map(part => normalizedKeyboardSegmentLookup[part.toLowerCase()] ?? part)
  const modifiers = parts.filter(isModifier)
  const key = parts.find(part => !isModifier(part))
  return { key: key ?? '', modifiers }
}

/** Parse a keybind string into a {@link Mousebind} if the key name describes a mouse button,
 * otherwise parse it into a {@link Keybind}. */
export function parseKeybindString(string: string): Keybind | Mousebind {
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
      key: KeyName(decomposed.key.toLowerCase()),
      modifierFlags: modifierFlagsForModifiers(decomposed.modifiers),
    }
  }
}
