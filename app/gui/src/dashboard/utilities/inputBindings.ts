/**
 * @file Exports `defineKeybinds`, a function to define a namespace containing keyboard and mouse
 * shortcuts.
 */
import { isElementTextInput, isTextInputEvent } from '#/utilities/event'
import { camelCaseToTitleCase } from '#/utilities/string'
import type { Icon } from '@/util/iconMetadata/iconName'
import {
  modifierFlagsForEvent,
  parseKeybindString,
  POINTER_BUTTON_FLAG,
  type AutocompleteKeybinds,
  type Modifier,
  type ModifierFlags,
  type ModifierKey,
  type PointerButtonFlags,
} from '@/util/shortcuts'
import { unsafeMutable } from 'enso-common/src/utilities/data/object'
import { isOnMacOS } from 'enso-common/src/utilities/detect'

/** The target of a {@link KeyboardEvent}, {@link MouseEvent}, or {@link PointerEvent}. */
export interface InputEventTarget<
  EventName extends string,
  Event extends
    | KeyboardEvent
    | MouseEvent
    | PointerEvent
    | React.KeyboardEvent
    | React.MouseEvent
    | React.PointerEvent,
> {
  readonly addEventListener: (eventName: EventName, handler: (event: Event) => void) => void
  readonly removeEventListener: (eventName: EventName, handler: (event: Event) => void) => void
}

/* eslint-disable @typescript-eslint/naming-convention */
const MODIFIER_FLAG_NAME: Readonly<Record<Modifier, ModifierKey>> = {
  Mod: isOnMacOS() ? 'Meta' : 'Ctrl',
  Alt: 'Alt',
  Shift: 'Shift',
}

/** Returns the raw modifier key equivalent of a modifier. */
export function toModifierKey(modifier: Modifier): ModifierKey {
  return MODIFIER_FLAG_NAME[modifier]
}

/** Return the equivalent {@link PointerButtonFlags} for the given mouse `button`. */
function buttonToPointerButtonFlags(button: number) {
  switch (button) {
    case 0: {
      return POINTER_BUTTON_FLAG.PointerMain
    }
    case 1: {
      return POINTER_BUTTON_FLAG.PointerAux
    }
    case 2: {
      return POINTER_BUTTON_FLAG.PointerSecondary
    }
    case 3: {
      return POINTER_BUTTON_FLAG.PointerBack
    }
    case 4: {
      return POINTER_BUTTON_FLAG.PointerForward
    }
    default: {
      // eslint-disable-next-line no-restricted-syntax
      return 0 as PointerButtonFlags
    }
  }
}

/** A list of keybinds, with metadata describing its purpose. */
export interface KeybindsWithMetadata<Category extends string> {
  readonly bindings: readonly [] | readonly string[]
  readonly category: Category
  readonly description?: string
  readonly icon?: string
  readonly color?: string
  /** Defaults to `true`. */
  readonly rebindable?: boolean
}

/**
 * A helper type used to autocomplete and validate an array of keyboard shortcuts (and its
 * associated metadata) in the editor.
 *
 * This type SHOULD NOT be explicitly written - it is only exported to suppress TypeScript
 * errors.
 */
export interface AutocompleteKeybindsWithMetadata<
  T extends KeybindsWithMetadata<Category>,
  Category extends string,
> {
  readonly bindings: AutocompleteKeybinds<T['bindings']>
  readonly category: Category
  readonly description?: string
  readonly icon?: Icon
  readonly color?: string
  /** Defaults to `true`. */
  readonly rebindable?: boolean
}

/** All the corresponding value for an arbitrary key of a {@link Keybinds}. */
type KeybindValue = KeybindsWithMetadata<string> | readonly [] | readonly string[]

/**
 * A helper type used to autocomplete and validate an object containing actions and their
 * corresponding keyboard shortcuts.
 */
// `never extends T ? Result : InferenceSource` is a trick to unify `T` with the actual type of the
// argument.
type Keybinds<T extends Record<keyof T, KeybindValue>, Category extends string> =
  never extends T ?
    {
      [K in keyof T]: T[K] extends readonly string[] ? AutocompleteKeybinds<T[K]>
      : T[K] extends KeybindsWithMetadata<Category> ?
        AutocompleteKeybindsWithMetadata<T[K], Category>
      : KeybindsWithMetadata<Category>
    }
  : T

const DEFINED_NAMESPACES = new Map<
  string,
  // This is SAFE, as the value is only being stored for bookkeeping purposes.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  ReturnType<typeof defineBindingNamespace<Record<any, any>, string>>
>()

export const DEFAULT_HANDLER = Symbol('default handler')

/**
 * Define key bindings for the given namespace.
 *
 * This function takes list of actions with default bindings, and returns an object which allows
 * making event handler which in turn may be added as an appropriate event listener. It may handle
 * both keyboard and mouse events.
 *
 * The event handler assigns functions to the corresponding action. The function may return false
 * if the event should be considered not handled (and thus propagated). Returning true or just
 * nothing from the function will cause propagation of event stop.
 * @param namespace - should be unique among other `defineKeybinds` calls.
 * @param originalBindings - an object defining actions and their key bindings. Each property name
 * is an action name, and the value is a list of default key bindings. See "Keybinds should be
 * parsed correctly" test for examples of valid strings.
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
 * useEvent(eventRegistry, 'keydown', graphBindingsHandler)
 * ```
 */
export function defineBindingNamespace<
  T extends Record<keyof T, KeybindValue>,
  Category extends string,
>(
  namespace: string,
  originalBindings: Keybinds<T, Category>,
  _categories: readonly Category[] | [],
) {
  /** The name of a binding in this set of keybinds. */
  type BindingKey = string & keyof T
  let keyboardShortcuts: Partial<Record<string, Partial<Record<ModifierFlags, Set<BindingKey>>>>> =
    {}
  let mouseShortcuts: Partial<
    Record<PointerButtonFlags, Partial<Record<ModifierFlags, Set<BindingKey>>>>
  > = []

  const bindings = structuredClone(originalBindings)
  // This is SAFE, as it is a `readonly` upcast.
  const bindingsAsRecord =
    // eslint-disable-next-line no-restricted-syntax
    bindings as Readonly<Record<string, KeybindValue>>

  // This non-null assertion is SAFE, as it is immediately assigned by `rebuildMetadata()`.
  let metadata!: Record<BindingKey, KeybindsWithMetadata<Category>>
  const rebuildMetadata = () => {
    // This is SAFE, as this type is a direct mapping from `bindingsAsRecord`, which has `BindingKey`
    // as its keys.
    // eslint-disable-next-line no-restricted-syntax
    metadata = Object.fromEntries(
      Object.entries(bindingsAsRecord).map((kv) => {
        const [name, info] = kv
        if (Array.isArray(info)) {
          return [name, { name: camelCaseToTitleCase(name), bindings: structuredClone(info) }]
        } else {
          return [name, structuredClone(info)]
        }
      }),
    ) as Record<BindingKey, KeybindsWithMetadata<Category>>
  }

  const rebuildLookups = () => {
    rebuildMetadata()
    keyboardShortcuts = {}
    mouseShortcuts = []
    for (const [nameRaw, value] of Object.entries(bindingsAsRecord)) {
      const keybindStrings = 'bindings' in value ? value.bindings : value
      // This is SAFE, as `Keybinds<T>` is a type derived from `T`.
      // eslint-disable-next-line no-restricted-syntax
      const name = nameRaw as BindingKey
      for (const keybindString of keybindStrings) {
        const keybind = parseKeybindString(keybindString).bind
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
  }
  rebuildLookups()

  const handler = <
    Event extends
      | KeyboardEvent
      | MouseEvent
      | PointerEvent
      | React.KeyboardEvent
      | React.MouseEvent
      | React.PointerEvent,
  >(
    handlers: Partial<
      // This MUST be `void` to allow implicit returns.
      Record<
        BindingKey | typeof DEFAULT_HANDLER,
        // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
        (event: Event, matchingBindings: Set<BindingKey>) => boolean | void
      >
    >,
    stopAndPrevent = true,
  ): ((event: Event, stopAndPrevent?: boolean) => boolean) => {
    return (event, innerStopAndPrevent = stopAndPrevent) => {
      const eventModifierFlags = modifierFlagsForEvent(event)
      const matchingBindings =
        'key' in event ?
          keyboardShortcuts[event.key.toLowerCase()]?.[eventModifierFlags]
        : mouseShortcuts[
            event.buttons !== 0 ?
              // eslint-disable-next-line no-restricted-syntax
              (event.buttons as PointerButtonFlags)
            : buttonToPointerButtonFlags(event.button)
          ]?.[eventModifierFlags]
      const shouldIgnoreEvent =
        'key' in event &&
        (event.key === 'Enter' || isTextInputEvent(event)) &&
        isElementTextInput(document.activeElement)
      let handle = shouldIgnoreEvent ? null : handlers[DEFAULT_HANDLER]
      if (matchingBindings != null && !shouldIgnoreEvent) {
        for (const bindingNameRaw in handlers) {
          // This is SAFE, because `handlers` is an object with identical keys to `T`,
          // which `BindingName` is also derived from.
          // eslint-disable-next-line no-restricted-syntax
          const bindingName = bindingNameRaw as BindingKey
          if (matchingBindings.has(bindingName)) {
            handle = handlers[bindingName]
            break
          }
        }
      }
      if (handle == null) {
        return false
      } else if (handle(event, matchingBindings ?? new Set()) === false) {
        return false
      } else {
        if (innerStopAndPrevent) {
          if ('stopImmediatePropagation' in event) {
            event.stopImmediatePropagation()
          } else {
            event.stopPropagation()
          }
          event.preventDefault()
        }
        return true
      }
    }
  }

  const defineHandlers = <
    Handlers extends Partial<
      // This MUST be `void` to allow implicit returns.
      Record<
        BindingKey | typeof DEFAULT_HANDLER,
        // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
        (event: Event, matchingBindings: Set<BindingKey>) => boolean | void
      >
    >,
  >(
    handlers: Handlers,
  ) => handlers

  const attach = <
    EventName extends string,
    Event extends
      | KeyboardEvent
      | MouseEvent
      | PointerEvent
      | React.KeyboardEvent
      | React.MouseEvent
      | React.PointerEvent,
  >(
    target: InputEventTarget<EventName, Event>,
    eventName: EventName,
    handlers: Partial<
      // This MUST be `void` to allow implicit returns.
      Record<
        BindingKey | typeof DEFAULT_HANDLER,
        // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
        (event: Event, matchingBindings: Set<BindingKey>) => boolean | void
      >
    >,
    stopAndPrevent = true,
  ) => {
    const newHandler = handler(handlers, stopAndPrevent)
    target.addEventListener(eventName, newHandler)
    return () => {
      target.removeEventListener(eventName, newHandler)
    }
  }

  const reset = (key: BindingKey) => {
    bindings[key] = structuredClone(originalBindings[key])
    rebuildLookups()
  }

  const deleteFunction = (key: BindingKey, binding: string) => {
    const bindingsOrInfo = bindingsAsRecord[key]
    const bindingsList =
      bindingsOrInfo != null && 'bindings' in bindingsOrInfo ?
        bindingsOrInfo.bindings
      : bindingsOrInfo
    if (bindingsList != null) {
      unsafeMutable(bindingsList).splice(bindingsList.indexOf(binding), 1)
      rebuildLookups()
    }
  }

  const add = (key: BindingKey, binding: string) => {
    const bindingsOrInfo = bindingsAsRecord[key]
    const bindingsList =
      bindingsOrInfo != null && 'bindings' in bindingsOrInfo ?
        bindingsOrInfo.bindings
      : bindingsOrInfo
    if (bindingsList != null) {
      unsafeMutable(bindingsList).push(binding)
      rebuildLookups()
    }
  }

  const result = {
    /** Return an event handler that handles a native keyboard, mouse or pointer event. */
    handler,
    defineHandlers,
    /**
     * Attach an event listener to an {@link EventTarget} and return a function to detach the
     * listener.
     */
    attach,
    /** Reset the entire list of bindings for a specific action to its default value. */
    reset,
    /** Delete one specific binding from the bindings for a specific action. */
    delete: deleteFunction,
    /** Add a new binding to the bindings for a specific action. */
    add,
    /** Metadata for every input binding. */
    get metadata() {
      return metadata
    },
    /** Add this namespace to the global lookup. */
    register: () => {
      if (DEFINED_NAMESPACES.has(namespace)) {
        // eslint-disable-next-line no-restricted-properties
        console.warn(
          `Overriding the keybind namespace '${namespace}', which has already been defined.`,
        )
        // eslint-disable-next-line no-restricted-properties
        console.trace()
      }
      DEFINED_NAMESPACES.set(namespace, result)
    },
    /** Remove this namespace from the global lookup. */
    unregister: () => {
      const cached = DEFINED_NAMESPACES.get(namespace)
      if (cached !== result) {
        return false
      } else {
        DEFINED_NAMESPACES.delete(namespace)
        return true
      }
    },
  } as const
  return result
}

/**
 * A function to define a bindings object that can be passed to {@link defineBindingNamespace}.
 * Useful when wanting to create reusable keybind definitions, or non-global keybind definitions.
 * @param categories - The categories of the bindings. Order will be preserved in the UI.
 * @param bindings - The bindings to define.
 * @returns An object containing the categories and bindings.
 */
export function defineBindings<T extends Record<keyof T, KeybindValue>, Category extends string>(
  categories: readonly Category[] | [],
  bindings: Keybinds<T, Category>,
) {
  return { categories, bindings }
}
