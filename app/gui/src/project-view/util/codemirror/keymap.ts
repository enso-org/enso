import { textEditorsCommonBindings } from '@/bindings'
import * as commands from '@codemirror/commands'
import { EditorView, type Command, type KeyBinding } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'

/**
 * Create a {@link KeyBinding} from an event handler compatible with those defined with our
 * `defineKeybinds` function.
 */
export function handlerToKeyBinding(
  handler: (event: KeyboardEvent, stopAndPrevent: boolean) => boolean,
  stopAndPrevent: boolean = false,
) {
  return {
    any: (_view: EditorView, event: KeyboardEvent) => handler(event, stopAndPrevent),
  }
}

function bindCommands<T extends string>(
  bindings: Record<T, Command>,
  view: EditorView,
): Record<T, () => boolean> {
  return objects.mapEntries(bindings, (_binding, command) => () => command(view))
}

/** @returns Key bindings applicable to all CodeMirror instances. */
export function baseKeymap(view: EditorView): KeyBinding[] {
  return [
    handlerToKeyBinding(
      textEditorsCommonBindings.handler(
        bindCommands(
          {
            moveLeft: commands.cursorCharLeft,
            moveRight: commands.cursorCharRight,
            deleteBack: commands.deleteCharBackward,
            deleteForward: commands.deleteCharForward,
          },
          view,
        ),
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

    {
      key: 'Enter',
      run: (view) => {
        view.contentDOM.blur()
        return true
      },
      preventDefault: true,
      stopPropagation: false,
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
}

/**
 * @returns Key bindings applicable to any CodeMirror instance that may be rendered as multi-line,
 * including both actual multi-line text and single-line text with the `lineWrapping` extension
 * enabled.
 */
export function verticalMovementKeymap(_view: EditorView): KeyBinding[] {
  return [
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
}
