import { valueExt } from '@/util/codemirror/stateEffect'
import { EditorView } from '@codemirror/view'
import { createDebouncer } from 'lib0/eventloop'

const {
  set: setContentFocused,
  get: contentFocused,
  changed: contentFocusedChanged,
  extension: valueExtension,
} = valueExt<boolean>(false)

export { contentFocused, contentFocusedChanged, setContentFocused }

/**
 * A CodeMirror extension enabling other extensions to respond to whether the editor content is
 * focused.
 *
 * The state field is updated asynchronously based on DOM focusin/focusout events.
 */
export function contentFocusedExt() {
  // It might be preferable to dispatch the updates synchronously, for a more predictable order of
  // event handlers; however, this would cause problems because the `mousedown` handler defined in
  // CodeMirror's `MouseSelection` can focus the editor explicitly *before* running the rest of the
  // mouse selection logic. This results in an inversion of the usual order of events: The `focusin`
  // handler is run before the `mousedown` handler finishes, so that a transaction with the
  // `pointer.select` user-event attribute may be received after the `focusin` event that it caused,
  // making it impossible to tell whether the element was already focused when the selection was
  // changed. By dispatching the transaction asynchronously, it is handled after the `mousedown`
  // event even if the `mousedown` handler explicitly focuses the element.
  const debounce = createDebouncer(0)
  let focused = false
  function observeFocus(view: EditorView) {
    if (view.state.field(contentFocused) === focused) return
    view.dispatch({ effects: setContentFocused.of(focused) })
  }
  return [
    valueExtension,
    // `EditorView.focusChangeEffect` serves a similar purpose, but I found it unsuitable, as in
    // some cases it can skip emitting a `StateEffect`: When focus changes, `EditorView.update`
    // creates a transaction applying any `StateEffect`s produced by the facet; it schedules this
    // transaction to be dispatched asynchronously. If there are any intervening transactions, the
    // transaction's `startState` doesn't match the current state, and can't be applied; in that
    // case the implementation silently drops it.
    EditorView.domEventObservers({
      focusin: (_event, view) => {
        focused = true
        debounce(() => observeFocus(view))
      },
      focusout: (_event, view) => {
        focused = false
        debounce(() => observeFocus(view))
      },
    }),
  ]
}
