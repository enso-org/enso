import { assert } from '@/util/assert'
import { unsafeKeys } from 'enso-common/src/utilities/data/object'
import { isOnMacOS } from 'enso-common/src/utilities/detect'

/** All possible modifier keys. */
export type ModifierKey = keyof typeof RAW_MODIFIER_FLAG
const DEBUG_LOG = false

// ======================
// === Modifier flags ===
// ======================

const RAW_MODIFIER_FLAG = {
  Ctrl: 1 << 0,
  Alt: 1 << 1,
  Shift: 1 << 2,
  Meta: 1 << 3,
}

export const MODIFIER_FLAG: Record<Modifier, number> = {
  Mod: isOnMacOS() ? RAW_MODIFIER_FLAG.Meta : RAW_MODIFIER_FLAG.Ctrl,
  Alt: RAW_MODIFIER_FLAG.Alt,
  Shift: RAW_MODIFIER_FLAG.Shift,
}

/** A number representing the unique combination of modifier flags. */
function modifierFlagsForModifiers(modifiers: Modifier[]): ModifierFlags {
  let result = 0
  for (const modifier of modifiers) {
    result |= MODIFIER_FLAG[modifier]
  }
  return result as ModifierFlags
}

/** The names of all {@link Modifier}s in this {@link ModifierFlags}, in the OS' preferred order. */
export function modifiersForModifierFlags(modifierFlags: ModifierFlags): Modifier[] {
  return ALL_MODIFIERS.filter((modifier) => (MODIFIER_FLAG[modifier] & modifierFlags) !== 0)
}

/**
 * Any event that contains modifier keys. {@link KeyboardEvent}s and {@link MouseEvent}s fall into
 * this category.
 */
interface EventWithModifiers {
  ctrlKey: boolean
  altKey: boolean
  shiftKey: boolean
  metaKey: boolean
}

/** A number representing the unique combination of modifier flags for an event.. */
export function modifierFlagsForEvent(event: EventWithModifiers): ModifierFlags {
  return ((event.ctrlKey ? RAW_MODIFIER_FLAG.Ctrl : 0) |
    (event.altKey ? RAW_MODIFIER_FLAG.Alt : 0) |
    (event.shiftKey ? RAW_MODIFIER_FLAG.Shift : 0) |
    (event.metaKey ? RAW_MODIFIER_FLAG.Meta : 0)) as ModifierFlags
}

/**
 * These values MUST match the flags on `MouseEvent#buttons`.
 * See https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons
 */
export const POINTER_BUTTON_FLAG = {
  PointerMain: 1 << 0,
  PointerSecondary: 1 << 1,
  PointerAux: 1 << 2,
  PointerBack: 1 << 3,
  PointerForward: 1 << 4,
} satisfies Record<Pointer, number> as Record<Pointer, PointerButtonFlags>

/**
 * Mapping from the MouseEvent's `button` field to PointerButtonFlags.
 *
 * No, it is not as simple as (1 << event.button) as PointerButtonFlags; compare
 * https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons with
 * https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button
 */
const flagsOfButtonField = [
  POINTER_BUTTON_FLAG.PointerMain,
  POINTER_BUTTON_FLAG.PointerAux,
  POINTER_BUTTON_FLAG.PointerSecondary,
  POINTER_BUTTON_FLAG.PointerBack,
  POINTER_BUTTON_FLAG.PointerForward,
]

function buttonFlagsForEvent(event: MouseEvent | TouchEvent | PointerEvent): PointerButtonFlags {
  if ('TouchEvent' in window && event instanceof TouchEvent) return POINTER_BUTTON_FLAG.PointerMain
  DEV: assert(event instanceof MouseEvent || event instanceof PointerEvent)
  // event.buttons keeps information about buttons being pressed, but in case of `click` or
  // `pointerup` events we also want to know what buttons were just released.
  return (event.buttons | (flagsOfButtonField[event.button] ?? 0)) as PointerButtonFlags
}

/** `event.button` and `event.buttons` fields for MouseEvent. Useful for creating mock events in tests. */
export function pointerButtonToEventInfo(key: Pointer): {
  button: number
  buttons: PointerButtonFlags
} {
  const buttons = POINTER_BUTTON_FLAG[key]
  const button = flagsOfButtonField.findIndex((flags) => flags === buttons)
  assert(
    button !== -1,
    'Can’t find corresponding event.button value for event.buttons: ${buttons}.',
  )
  return { button, buttons }
}

// ==========================
// === Autocomplete types ===
// ==========================

// 'Mod' functions Cmd on MacOS and Ctrl on every other platform.
const ALL_MODIFIERS = ['Mod', 'Alt', 'Shift'] as const
export type Modifier = (typeof ALL_MODIFIERS)[number]
type LowercaseModifier = Lowercase<Modifier>
const ALL_POINTERS = [
  'PointerMain',
  'PointerSecondary',
  'PointerAux',
  'PointerBack',
  'PointerForward',
] as const
/** All valid mouse pointer buttons. */
export type Pointer = (typeof ALL_POINTERS)[number]
type LowercasePointer = Lowercase<Pointer>
// prettier-ignore
/** This list is non-exhaustive. It is intentionally limited to keys found on most keyboards. */
const ALL_KEYS = [
  'Escape', 'Enter', 'Backspace', 'Insert', 'Delete',
  'OsDelete', // The key labeled as `Delete` - `Backspace` on macOS, `Delete` on all other platforms.
  'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight',
  'Home', 'End', 'PageUp', 'PageDown', 'Tab', 'Space',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '`', '-', '=', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_',  '+',
  '[', ']', '\\', '{', '}', '|', ';', "'", ':', '"', ',', '.', '/', '<', '>', '?',
  'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12',
  // Not part of most layouts, but can still be manually mapped by custom keyboards or OS tools
] as const
/** Common keyboard keys. */
type Key = (typeof ALL_KEYS)[number]
type LowercaseKey = Lowercase<Key>
type KeybindSegment = Modifier | Pointer | Key
/** @internal */
export const normalizedKeyboardSegmentLookup = Object.fromEntries<string>(
  [...ALL_MODIFIERS, ...ALL_POINTERS, ...ALL_KEYS].map((entry) => [entry.toLowerCase(), entry]),
)
normalizedKeyboardSegmentLookup[''] = '+'
normalizedKeyboardSegmentLookup['space'] = ' '
normalizedKeyboardSegmentLookup['osdelete'] = isOnMacOS() ? 'Backspace' : 'Delete'
/**
 * A mapping between the lowercased segment of a keyboard shortcut to its properly capitalized
 * normalized form.
 */
type NormalizeKeybindSegment = {
  [K in KeybindSegment as Lowercase<K>]: K
}
/** A segment suggestible by autocomplete. */
type SuggestedKeybindSegment = Key | Pointer | `${Modifier}+`
/** A helper type used to autocomplete and validate a single keyboard shortcut in the editor. */
export type AutocompleteKeybind<T extends string, FoundKeyName extends string = never> =
  T extends '+' ? T
  : T extends `${infer First}+${infer Rest}` ?
    Lowercase<First> extends LowercaseModifier ?
      `${NormalizeKeybindSegment[Lowercase<First>] & string}+${AutocompleteKeybind<Rest>}`
    : Lowercase<First> extends LowercasePointer | LowercaseKey ?
      AutocompleteKeybind<Rest, NormalizeKeybindSegment[Lowercase<First>] & string>
    : `${Modifier}+${AutocompleteKeybind<Rest>}`
  : T extends '' ? SuggestedKeybindSegment
  : Lowercase<T> extends LowercasePointer | LowercaseKey ? NormalizeKeybindSegment[Lowercase<T>]
  : Lowercase<T> extends LowercaseModifier ?
    [FoundKeyName] extends [never] ?
      `${NormalizeKeybindSegment[Lowercase<T>] & string}+${SuggestedKeybindSegment}`
    : `${NormalizeKeybindSegment[Lowercase<T>] & string}+${FoundKeyName}`
  : [FoundKeyName] extends [never] ? SuggestedKeybindSegment
  : FoundKeyName

export type AutocompleteKeybinds<T extends readonly KeybindDefinition[]> = {
  [K in keyof T]: T[K] extends FullKeybindDefinition ?
    FullKeybindDefinition<AutocompleteKeybind<T[K]['key']>>
  : T[K] extends string ? AutocompleteKeybind<T[K]>
  : never
}

// `never extends T ? Result : InferenceSource` is a trick to unify `T` with the actual type of the
// argument.
type Keybinds<T extends Record<K, KeybindDefinition[]>, K extends keyof T = keyof T> =
  never extends T ?
    {
      [K in keyof T]: AutocompleteKeybinds<T[K]>
    }
  : T

declare const brandKey: unique symbol
type Key_ = string & { [brandKey]: never }
declare const brandModifierFlags: unique symbol
export type ModifierFlags = number & { [brandModifierFlags]: never }
declare const brandPointerButtonFlags: unique symbol
export type PointerButtonFlags = number & { [brandPointerButtonFlags]: never }

const definedNamespaces = new Set<string>()

export const DefaultHandler = Symbol('default handler')

interface KeybindOptions {
  allowRepeat?: boolean
}
export interface FullKeybindDefinition<T = string> extends KeybindOptions {
  key: T
}
export type KeybindDefinition = string | FullKeybindDefinition

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
 * @param namespace should be unique among other `defineKeybinds` calls.
 * @param bindings is an object defining actions and their key bindings. Each property name is an
 * action name, and value is list of default key bindings. See "Keybinds should be parsed
 * correctly" test for examples of valid strings.
 * @returns an object with defined `handler` function and `bindings`,
 * containing information about assigned bindings.
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
 * useEvent(eventRegistry, 'keydown', graphBindingsHandler)
 * ```
 *
 * Use `bindingsInfo` to display the current binding in UI:
 * ```
 * const label = graphBindings.bindings.undo.humanReadable
 * ```
 */
export function defineKeybinds<
  T extends Record<BindingName, [] | KeybindDefinition[]>,
  BindingName extends keyof T = keyof T,
>(namespace: string, bindings: Keybinds<T>) {
  if (definedNamespaces.has(namespace)) {
    console.warn(`The keybind namespace '${namespace}' has already been defined.`)
  } else {
    definedNamespaces.add(namespace)
  }
  const keyboardShortcuts: Partial<Record<Key_, Record<ModifierFlags, Set<BindingName>>>> = {}
  const mouseShortcuts: Record<PointerButtonFlags, Record<ModifierFlags, Set<BindingName>>> = []

  function fullKeybind(keybind: KeybindDefinition): FullKeybindDefinition {
    return typeof keybind === 'string' ? { key: keybind } : keybind
  }

  const bindingsInfo = {} as Record<BindingName, BindingInfo>
  const bindingsOptions = {} as Record<BindingName, KeybindOptions>
  for (const [name_, keybindValues] of Object.entries(bindings)) {
    const name = name_ as BindingName
    for (const keybindValue of keybindValues as KeybindDefinition[]) {
      const keybindDef = fullKeybind(keybindValue)
      const { bind: keybind, info } = parseKeybindString(keybindDef.key)
      if (bindingsInfo[name] == null) {
        bindingsInfo[name] = info
        bindingsOptions[name] = keybindDef
      }
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

  function eventKey(event: KeyboardEvent): Key_ {
    // On OS X, the `option` modifier causes keys to be interpreted as language-specific alternative
    // characters. Ideally, we would identify the key pressed if `option` were not held, since we
    // treat `option` as a modifier of the base key; however, the event API does not provide a way
    // to do this.
    // As a workaround to support `Alt+Digit` bindings, in case the physical key pressed is a digit,
    // we use the physical key code instead. This would not be a suitable solution for most keys,
    // since the physical key `code` doesn't respect the user's layout, but very few users are
    // likely to use keyboard layouts that change the interpretation of the digits (the "original"
    // Dvorak layout does this, but even Dvorak users mostly use a variant that leaves the digits in
    // numeric order).
    const digit = event.code.match(/Digit(\d)/)
    if (digit) return digit[1] as Key_
    // If the physical key is not a digit, we use the `key` field, which respects the user's layout.
    return event.key.toLowerCase() as Key_
  }

  function handler<Event_ extends AnyHandlerEvent>(
    handlers: Partial<
      Record<BindingName | typeof DefaultHandler, (event: Event_) => boolean | void>
    >,
  ): (event: Event_) => boolean {
    return (event) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const keybinds =
        event instanceof KeyboardEvent ? keyboardShortcuts[eventKey(event)]?.[eventModifierFlags]
          // Chrome sometimes sends `keydown` which is not of type `KeyboardEvent`. For now, we
          // ignore them, as these are unusual (like picking a cached form value).
        : event instanceof MouseEvent || event instanceof PointerEvent ?
          mouseShortcuts[buttonFlagsForEvent(event)]?.[eventModifierFlags]
        : undefined

      const isRepeat = event instanceof KeyboardEvent && event.repeat
      let handled = false
      if (keybinds != null) {
        for (const bindingName of unsafeKeys(handlers)) {
          if (isRepeat && !bindingsOptions[bindingName].allowRepeat) continue
          if (keybinds.has(bindingName)) {
            const handle = handlers[bindingName as BindingName]
            handled = handle && handle(event) !== false
            if (DEBUG_LOG)
              console.log(
                `Event ${event.type} (${event instanceof KeyboardEvent ? event.key : buttonFlagsForEvent(event)})`,
                `${handled ? 'handled' : 'processed'} by ${namespace}.${String(bindingName)}`,
              )
            if (handled) break
          }
        }
      }
      if (!handled && handlers[DefaultHandler] != null) {
        handled = handlers[DefaultHandler](event) !== false
      }
      if (handled) {
        event.stopImmediatePropagation()
        // We don't prevent default on PointerEvents, because it may prevent emitting
        // mousedown/mouseup events, on which external libraries may rely (like AGGrid for hiding
        // context menu).
        if (!(event instanceof PointerEvent)) event.preventDefault()
      }
      return handled
    }
  }

  return { handler, bindings: bindingsInfo }
}

/** A type predicate that narrows the potential child of the array. */
function includesPredicate<T extends U, U>(array: readonly T[]) {
  const array_: readonly unknown[] = array
  return (element: unknown): element is T => array_.includes(element)
}

export const isModifier = includesPredicate(ALL_MODIFIERS)
export const isPointer = includesPredicate(ALL_POINTERS)

/**
 * Convert a keybind string to an intermediate form containing both the key and its modifiers
 * (if any).
 *  @internal
 */
export function decomposeKeybindString(string: string): ModifierStringDecomposition {
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

/**
 * Parse a keybind string into a {@link Mousebind} if the key name describes a mouse button,
 * otherwise parse it into a {@link Keybind}.
 */
export function parseKeybindString(string: string): {
  bind: Keybind | Mousebind
  info: BindingInfo
} {
  const decomposed = decomposeKeybindString(string)
  const humanReadableSegments = [...decomposed.modifiers, decomposed.key].map(humanReadableKey)
  const info = {
    humanReadableSegments,
    humanReadable: humanReadableSegments.join(' + '),
    key: decomposed.key,
    modifiers: decomposed.modifiers,
  }
  if (isPointer(decomposed.key)) {
    return {
      info,
      bind: {
        type: 'mousebind',
        key: POINTER_BUTTON_FLAG[decomposed.key],
        modifierFlags: modifierFlagsForModifiers(decomposed.modifiers),
      },
    }
  } else {
    return {
      info,
      bind: {
        type: 'keybind',
        key: decomposed.key.toLowerCase() as Key_,
        modifierFlags: modifierFlagsForModifiers(decomposed.modifiers),
      },
    }
  }
}

/** Pointers and some keys have not human-friendly name, these are overwritten here for {@link BindingInfo}. */
const HUMAN_READABLE_POINTERS_AND_KEYS: Map<string, string> = new Map(
  Object.entries({
    PointerMain: 'Click',
    PointerSecondary: 'Right click',
    PointerAux: 'Middle click',
    PointerBack: 'Mouse Back',
    PointerForward: 'Mouse Forward',
    ArrowLeft: 'Arrow left',
    ArrowRight: 'Arrow right',
    ArrowUp: 'Arrow up',
    ArrowDown: 'Arrow down',
    PageUp: 'Page up',
    PageDown: 'Page down',
    ' ': 'Space',
    Mod: isOnMacOS() ? 'Cmd' : 'Ctrl',
    Alt: isOnMacOS() ? 'Option' : 'Alt',
  }),
)

function humanReadableKey(key: string): string {
  return HUMAN_READABLE_POINTERS_AND_KEYS.get(key) ?? key
}

/** Information about binding for displaying to the user or usage in tests. */
export interface BindingInfo {
  /** Human-readable representation of keys and modifiers in the binding. No specific format. */
  humanReadable: string
  /** Human-readable representation of keys and modifiers in the binding, key by key. */
  humanReadableSegments: string[]
  /** The key of a binding. */
  key: string
  /** The list of modifiers in the binding. */
  modifiers: Modifier[]
}

/**
 * An intermediate representation of a keybind, in which all segments have been tokenized but
 * before converting into either a {@link Keybind} or a {@link Mousebind}.
 */
interface ModifierStringDecomposition {
  key: string
  modifiers: Modifier[]
}

/** A keyboard shortcut. */
interface Keybind {
  type: 'keybind'
  key: Key_
  modifierFlags: ModifierFlags
}

/** A mouse shortcut. */
interface Mousebind {
  type: 'mousebind'
  key: PointerButtonFlags
  modifierFlags: ModifierFlags
}

export type AnyHandlerEvent = KeyboardEvent | MouseEvent | PointerEvent | TouchEvent

/** A handler stopping event if it's browser's "copy" shortcut. */
export function stopCopy(event: KeyboardEvent) {
  if (modifierFlagsForEvent(event) === MODIFIER_FLAG.Mod && event.key.toLowerCase() === 'c') {
    event.stopPropagation()
  }
}
