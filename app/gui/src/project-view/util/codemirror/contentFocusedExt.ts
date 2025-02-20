import { valueExt } from '@/util/codemirror/stateEffect'
import { EditorView } from '@codemirror/view'

const {
  set: setContentFocused,
  get: contentFocused,
  changed: contentFocusedChanged,
  extension: valueExtension,
} = valueExt<boolean>(false)

export { contentFocused, contentFocusedChanged }

/** A CodeMirror extension enabling other extensions to respond to whether the editor content is focused. */
export function contentFocusedExt() {
  return [
    valueExtension,
    EditorView.focusChangeEffect.of((_state, focusing) => setContentFocused.of(focusing)),
  ]
}
