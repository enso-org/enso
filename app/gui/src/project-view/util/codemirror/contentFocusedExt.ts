import { valueExt } from '@/util/codemirror/stateEffect'
import type { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'

/**
 * A CodeMirror extension enabling other extensions to respond to whether the editor content is
 * focused.
 *
 * The state field is updated asynchronously based on DOM focusin/focusout events. Although
 * dispatching the updates synchronously would provide a more predictable order of event handlers,
 * this would cause problems because the `mousedown` handler defined in CodeMirror's
 * `MouseSelection` can focus the editor explicitly *before* running the rest of the mouse selection
 * logic. This results in an inversion of the usual order of events: The `focusin` handler is run
 * before the `mousedown` handler finishes, so that a transaction with the `pointer.select`
 * user-event attribute may be received after the `focusin` event that it caused, making it
 * impossible to tell whether the element was already focused when the selection was changed. By
 * dispatching the transaction asynchronously, it is handled after the `mousedown` event even if the
 * `mousedown` handler explicitly focuses the element.
 *
 * This extension is similar to {@link EditorView.focusChangeEffect}, but more reliable. In some
 * cases `focusChangeEffect` it can skip emitting a {@link StateEffect}: When focus changes,
 * `EditorView.update` creates a transaction applying any `StateEffect`s produced by the facet; it
 * schedules this transaction to be dispatched asynchronously. If there are any intervening
 * transactions, the transaction's `startState` doesn't match the current state, and can't be
 * applied; in that case the implementation silently drops it.
 */
export function contentFocusedExt(): Extension {
  return extInstance
}

const {
  set: setContentFocused,
  get: contentFocused,
  changed: contentFocusedChanged,
  extension: valueExtension,
} = valueExt<boolean>(false)
export { contentFocused, contentFocusedChanged, setContentFocused }

function observeFocus(view: EditorView, focused: boolean) {
  view.dispatch({ effects: setContentFocused.of(focused) })
}
const extInstance: Extension = [
  valueExtension,
  EditorView.domEventObservers({
    focusin: (_event, view) => {
      setTimeout(() => observeFocus(view, true))
    },
    focusout: (_event, view) => {
      setTimeout(() => observeFocus(view, false))
    },
  }),
]
