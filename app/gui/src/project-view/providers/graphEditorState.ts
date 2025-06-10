import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import { Ref } from 'vue'

/** The global state of the graph editor. */
export interface GraphEditorState {
  /** Whether the component browser is opened or not. */
  componentBrowserOpened: Ref<boolean>
}

export { provideGraphEditorState, useGraphEditorState }
const [provideGraphEditorState, useGraphEditorState] = createContextStore(
  'Graph editor state',
  identity<GraphEditorState>,
)
