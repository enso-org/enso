import { textEditorsCommonBindings, textEditorsMultilineBindings } from '@/bindings'
import * as commands from '@codemirror/commands'
import { insertNewlineKeepIndent } from '@codemirror/commands'
import { type Extension, Prec } from '@codemirror/state'
import { type Command, EditorView, type KeyBinding, keymap } from '@codemirror/view'
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
  return objects.mapEntries(
    bindings,
    (_binding, command) => (event: CmKeyboardEvent) => command(event.codemirrorView),
  )
}

/** Key bindings applicable to all CodeMirror instances. */
const baseKeymap: KeyBinding[] = [
  handlerToKeyBinding(
    textEditorsCommonBindings.handler(
      bindCommands({
        moveLeft: commands.cursorCharLeft,
        moveRight: commands.cursorCharRight,
        deleteBack: commands.deleteCharBackward,
        deleteForward: commands.deleteCharForward,
      }),
    ),
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
    key: 'Ctrl-b',
    run: commands.cursorCharLeft,
    shift: commands.selectCharLeft,
    preventDefault: true,
    stopPropagation: true,
  },
  {
    key: 'Ctrl-f',
    run: commands.cursorCharRight,
    shift: commands.selectCharRight,
    stopPropagation: true,
  },

  {
    key: 'Ctrl-a',
    run: commands.cursorLineStart,
    shift: commands.selectLineStart,
    stopPropagation: true,
  },
  {
    key: 'Ctrl-e',
    run: commands.cursorLineEnd,
    shift: commands.selectLineEnd,
    stopPropagation: true,
  },

  { key: 'Ctrl-d', run: commands.deleteCharForward, stopPropagation: true },
  { key: 'Ctrl-h', run: commands.deleteCharBackward, stopPropagation: true },
  { key: 'Ctrl-k', run: commands.deleteToLineEnd, stopPropagation: true },
  { key: 'Ctrl-Alt-h', run: commands.deleteGroupBackward, stopPropagation: true },

  { key: 'Ctrl-t', run: commands.transposeChars, stopPropagation: true },
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
    key: 'Ctrl-p',
    run: commands.cursorLineUp,
    shift: commands.selectLineUp,
    stopPropagation: true,
  },
  {
    key: 'Ctrl-n',
    run: commands.cursorLineDown,
    shift: commands.selectLineDown,
    stopPropagation: true,
  },

  { key: 'Ctrl-o', run: commands.splitLine, stopPropagation: true },
  { key: 'Ctrl-v', run: commands.cursorPageDown, stopPropagation: true },
]

const stopEvent = (event: Event) => {
  event.stopImmediatePropagation()
  return false
}

const autoOrMultiHandlers = handlerToKeyBinding(
  textEditorsMultilineBindings.handler({
    newline: (e) => {
      e.stopImmediatePropagation()
      return insertNewlineKeepIndent(e.codemirrorView)
    },
  }),
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
