import { $setRootText } from '@/components/lexical/utils'
import { $getRoot, type EditorState } from 'lexical'
import { useLexicalComposer } from 'lexical-vue'
import { computed, ref } from 'vue'

const SYNC_TAG = 'ENSO_SYNC'

function $getRootText() {
  return $getRoot().getTextContent()
}

/** Enables two-way synchronization between the editor and a string model `content`.
 *
 * By default, the editor's text contents are synchronized with the string. A content getter and setter may be provided
 * to synchronize a different view of the state, e.g. to transform to an encoding that keeps rich text information.
 */
export function useLexicalSync(
  $getEditorContent: () => string = $getRootText,
  $setEditorContent: (content: string) => void = $setRootText,
) {
  const editor = useLexicalComposer()

  const state = ref<EditorState>()

  const unregister = editor.registerUpdateListener(({ editorState, tags }) => {
    if (tags.has(SYNC_TAG)) return
    state.value = editorState
  })

  return {
    content: computed({
      get: () => {
        if (!state.value) return ''
        return state.value.read(() => $getEditorContent())
      },
      set: (content) => {
        editor.update(
          () => {
            if ($getEditorContent() !== content) $setEditorContent(content)
          },
          {
            discrete: true,
            skipTransforms: true,
            tag: SYNC_TAG,
          },
        )
      },
    }),
    unregister,
  }
}
