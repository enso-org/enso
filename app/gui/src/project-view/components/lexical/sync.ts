import { type ToValue } from '@/util/reactivity'
import type { LexicalEditor } from 'lexical'
import { $createParagraphNode, $createTextNode, $getRoot, $setSelection } from 'lexical'
import { computed, shallowRef, toValue } from 'vue'

const SYNC_TAG = 'ENSO_SYNC'

/**
 * Enables two-way synchronization between the editor and a string model `content`.
 *
 * By default, the editor's text contents are synchronized with the string. A content getter and setter may be provided
 * to synchronize a different view of the state, e.g. to transform to an encoding that keeps rich text information.
 */
export function useLexicalStringSync(
  editor: LexicalEditor,
  $getEditorContent: () => string = $getRootText,
  $setEditorContent: (content: string) => void = $setRootTextClearingSelection,
) {
  return useLexicalSync(editor, $getEditorContent, (content, prevContent) => {
    if (content !== toValue(prevContent)) $setEditorContent(content)
  })
}

/** TODO: Add docs */
export function useLexicalSync<T>(
  editor: LexicalEditor,
  $read: () => T,
  $write: (content: T, prevContent: ToValue<T>) => void,
) {
  const state = shallowRef(editor.getEditorState())

  const unregister = editor.registerUpdateListener(({ editorState, tags }) => {
    if (tags.has(SYNC_TAG)) return
    state.value = editorState
  })

  const getContent = () => editor.getEditorState().read($read)

  return {
    content: {
      editedContent: computed(() => state.value.read($read)),
      set: (content: T) => {
        editor.update(() => $write(content, getContent), {
          discrete: true,
          skipTransforms: true,
          tag: SYNC_TAG,
        })
      },
    },
    unregister,
  }
}

/** TODO: Add docs */
export function $getRootText() {
  return $getRoot().getTextContent()
}

/** TODO: Add docs */
export function $setRootText(text: string) {
  const root = $getRoot()
  root.clear()
  const paragraph = $createParagraphNode()
  paragraph.append($createTextNode(text))
  root.append(paragraph)
}

function $setRootTextClearingSelection(text: string) {
  $setRootText(text)
  $setSelection(null)
}
