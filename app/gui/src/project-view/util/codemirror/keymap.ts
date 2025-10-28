import { textEditorsCommonBindings, textEditorsMultilineBindings } from '@/bindings'
import { modKey } from '@/composables/events'
import * as commands from '@codemirror/commands'
import { insertNewlineKeepIndent } from '@codemirror/commands'
import { Prec, type Extension } from '@codemirror/state'
import { EditorView, keymap, type Command, type KeyBinding } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'
import type { LineMode } from './index'

export interface CmEvent {
  codemirrorView: EditorView
}

export type CmEventExt<T extends Event> = T & CmEvent

type CmKeyboardEvent = CmEventExt<KeyboardEvent>

/** Extend any kind of DOM event with a property holding a reference to codemirror view. */
export function extendCmEvent<E extends Event>(view: EditorView, event: E): CmEventExt<E> {
  const ext = event as CmEventExt<E>
  ext.codemirrorView = view
  return ext
}

const stopEvent = (event: Event) => {
  event.stopImmediatePropagation()
  return false
}

/**
 * Create a {@link KeyBinding} from an event handler compatible with those defined with our
 * `defineKeybinds` function.
 */
export function handlerToKeyBinding(
  handler: (event: CmKeyboardEvent, stopAndPrevent: boolean) => boolean,
  stopAndPrevent: boolean = false,
): KeyBinding {
  return {
    any: (view: EditorView, event: KeyboardEvent) =>
      handler(extendCmEvent(view, event), stopAndPrevent),
  }
}

function bindCommands<T extends string>(
  bindings: Record<T, Command>,
): Record<T, (event: CmKeyboardEvent) => boolean> {
  return objects.mapEntries(bindings, (_binding, command) => (event: CmKeyboardEvent) => {
    command(event.codemirrorView)
    // Some commands return `false` if not applicable to the current state; this allows falling
    // back to a lower-priority command, but we don't allow conditionally bubbling the event out
    // of the editor.
    return true
  })
}

function isNormalPrintableKey(event: KeyboardEvent) {
  // This condition matches most printable characters, but not Enter or Tab.
  return event.key.length === 1 && !modKey(event) && !event.altKey
}

function isModifierKey(key: string) {
  // This condition matches modifier keys that may be used as part of text editing and should be
  // stopped from propagating to ancestors.
  return ['Control', 'Alt', 'Meta', 'Shift'].includes(key)
}

const stopNormalKeys: KeyBinding[] = [
  {
    any: (_view, event: KeyboardEvent) => {
      // Stop propagation of typical keys that will result in a character being inserted into the
      // editor, and modifier keys occurring alone.
      if (isNormalPrintableKey(event) || isModifierKey(event.key)) event.stopImmediatePropagation()
      return false
    },
  },
]

/** Key bindings applicable to all CodeMirror instances. */
const baseKeymap: KeyBinding[] = [
  handlerToKeyBinding(
    textEditorsCommonBindings.handler({
      ...bindCommands({
        'textEditor.moveLeft': commands.cursorCharLeft,
        'textEditor.moveRight': commands.cursorCharRight,
        'textEditor.deleteBack': commands.deleteCharBackward,
        'textEditor.deleteForward': commands.deleteCharForward,
      }),
      'textEditor.copy': stopEvent,
      'textEditor.cut': stopEvent,
      'textEditor.paste': stopEvent,
    }),
    true,
  ),
  {
    key: 'ArrowLeft',
    shift: commands.selectCharLeft,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    key: 'Mod-ArrowLeft',
    mac: 'Alt-ArrowLeft',
    run: commands.cursorGroupLeft,
    shift: commands.selectGroupLeft,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    mac: 'Cmd-ArrowLeft',
    run: commands.cursorLineBoundaryLeft,
    shift: commands.selectLineBoundaryLeft,
    preventDefault: true,
    stopPropagation: true,
  },

  {
    key: 'ArrowRight',
    shift: commands.selectCharRight,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    key: 'Mod-ArrowRight',
    mac: 'Alt-ArrowRight',
    run: commands.cursorGroupRight,
    shift: commands.selectGroupRight,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    mac: 'Cmd-ArrowRight',
    run: commands.cursorLineBoundaryRight,
    shift: commands.selectLineBoundaryRight,
    preventDefault: true,
    stopPropagation: true,
  },

  {
    mac: 'Cmd-ArrowUp',
    run: commands.cursorDocStart,
    shift: commands.selectDocStart,
    stopPropagation: true,
  },

  {
    mac: 'Cmd-ArrowDown',
    run: commands.cursorDocEnd,
    shift: commands.selectDocEnd,
    stopPropagation: true,
  },

  {
    key: 'Home',
    run: commands.cursorLineBoundaryBackward,
    shift: commands.selectLineBoundaryBackward,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    key: 'Mod-Home',
    run: commands.cursorDocStart,
    shift: commands.selectDocStart,
    stopPropagation: true,
  },

  {
    key: 'End',
    run: commands.cursorLineBoundaryForward,
    shift: commands.selectLineBoundaryForward,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    key: 'Mod-End',
    run: commands.cursorDocEnd,
    shift: commands.selectDocEnd,
    stopPropagation: true,
  },

  { key: 'Mod-a', run: commands.selectAll, stopPropagation: true },

  {
    key: 'Mod-Backspace',
    mac: 'Alt-Backspace',
    run: commands.deleteGroupBackward,
    stopPropagation: true,
  },
  {
    key: 'Mod-Delete',
    mac: 'Alt-Delete',
    run: commands.deleteGroupForward,
    stopPropagation: true,
  },
  { mac: 'Mod-Backspace', run: commands.deleteLineBoundaryBackward, stopPropagation: true },
  { mac: 'Mod-Delete', run: commands.deleteLineBoundaryForward, stopPropagation: true },

  {
    mac: 'Ctrl-b',
    run: commands.cursorCharLeft,
    shift: commands.selectCharLeft,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    mac: 'Ctrl-f',
    run: commands.cursorCharRight,
    shift: commands.selectCharRight,
    stopPropagation: true,
  },

  {
    mac: 'Ctrl-a',
    run: commands.cursorLineStart,
    shift: commands.selectLineStart,
    stopPropagation: true,
  },
  {
    mac: 'Ctrl-e',
    run: commands.cursorLineEnd,
    shift: commands.selectLineEnd,
    stopPropagation: true,
  },

  { mac: 'Ctrl-d', run: commands.deleteCharForward, stopPropagation: true },
  { mac: 'Ctrl-h', run: commands.deleteCharBackward, stopPropagation: true },
  { mac: 'Ctrl-k', run: commands.deleteToLineEnd, stopPropagation: true },
  { mac: 'Ctrl-Alt-h', run: commands.deleteGroupBackward, stopPropagation: true },

  { mac: 'Ctrl-t', run: commands.transposeChars, stopPropagation: true },
]

/** Bindings applicable in text that is always single-line, or single-line by default. */
const nonMultilineKeymap: KeyBinding[] = [
  {
    key: 'Enter',
    run: (view) => {
      view.contentDOM.blur()
      return true
    },
    preventDefault: true,
    stopPropagation: false,
  },
]

/** Bindings applicable in text that is always multi-line, or multi-line by default. */
const multilineKeymap: KeyBinding[] = [
  {
    key: 'Enter',
    run: commands.insertNewline,
    preventDefault: true,
    stopPropagation: true,
  },
]

/**
 * Key bindings applicable to any CodeMirror instance that may be rendered as multi-line,
 * including both actual multi-line text and single-line text with the `lineWrapping` extension
 * enabled.
 */
export const verticalMovementKeymap: KeyBinding[] = [
  {
    key: 'ArrowUp',
    run: commands.cursorLineUp,
    shift: commands.selectLineUp,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    mac: 'Ctrl-ArrowUp',
    run: commands.cursorPageUp,
    shift: commands.selectPageUp,
    stopPropagation: true,
  },

  {
    key: 'ArrowDown',
    run: commands.cursorLineDown,
    shift: commands.selectLineDown,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    mac: 'Ctrl-ArrowDown',
    run: commands.cursorPageDown,
    shift: commands.selectPageDown,
    stopPropagation: true,
  },
  {
    key: 'PageUp',
    run: commands.cursorPageUp,
    shift: commands.selectPageUp,
    stopPropagation: true,
  },
  {
    key: 'PageDown',
    run: commands.cursorPageDown,
    shift: commands.selectPageDown,
    stopPropagation: true,
  },

  {
    mac: 'Ctrl-p',
    run: commands.cursorLineUp,
    shift: commands.selectLineUp,
    stopPropagation: true,
  },
  {
    mac: 'Ctrl-n',
    run: commands.cursorLineDown,
    shift: commands.selectLineDown,
    stopPropagation: true,
  },

  { mac: 'Ctrl-o', run: commands.splitLine, stopPropagation: true },
  { mac: 'Ctrl-v', run: commands.cursorPageDown, stopPropagation: true },
]

const autoOrMultiHandlers = handlerToKeyBinding(
  textEditorsMultilineBindings.handler(
    bindCommands({
      'textEditor.newline': insertNewlineKeepIndent,
    }),
  ),
  true,
)

const standardBindings: Record<LineMode, KeyBinding[]> = {
  single: nonMultilineKeymap,
  auto: [autoOrMultiHandlers, ...nonMultilineKeymap],
  autoMulti: [autoOrMultiHandlers, ...nonMultilineKeymap, ...verticalMovementKeymap],
  multi: [autoOrMultiHandlers, ...multilineKeymap, ...verticalMovementKeymap],
}

function makeBindingsExt(lineMode: LineMode, extras?: Extension[]): Extension {
  return [
    Prec.lowest(keymap.of(baseKeymap)),
    Prec.lowest(keymap.of(stopNormalKeys)),
    Prec.low(keymap.of(standardBindings[lineMode])),
    ...(extras ?? []),
  ]
}

const stopWheel = EditorView.domEventHandlers({ wheel: stopEvent })

const bindingsExt = {
  single: makeBindingsExt('single'),
  auto: makeBindingsExt('auto'),
  autoMulti: makeBindingsExt('autoMulti', [stopWheel]),
  multi: makeBindingsExt('multi', [stopWheel]),
}

/** @returns An extension implementing the key bindings for the given line mode. */
export function keyBindings(lineMode: LineMode): Extension {
  return bindingsExt[lineMode]
}
