import { type GraphStore } from '@/stores/graph'
import { type ProjectStore } from '@/stores/project'
import { changeSetToTextEdits } from '@/util/codemirror/text'
import { useToast } from '@/util/toast'
import {
  Annotation,
  type ChangeSet,
  type EditorSelection,
  type Extension,
  type Text,
} from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { createDebouncer } from 'lib0/eventloop'
import { onUnmounted, watch } from 'vue'
import { type SourceRangeEdit, textChangeToEdits } from 'ydoc-shared/util/data/text'
import { type Origin } from 'ydoc-shared/yjsModel'

// Indicates a change updating the text to correspond to the given module state.
const synchronizedModule = Annotation.define<true>()

/** @returns A CodeMirror Extension that synchronizes the editor state with the AST of an Enso module. */
export function useEnsoSourceSync(
  projectStore: Pick<ProjectStore, 'module'>,
  graphStore: Pick<
    GraphStore,
    'moduleSource' | 'viewModule' | 'startEdit' | 'commitEdit' | 'onBeforeEdit'
  >,
  editorView: EditorView,
) {
  let pendingChanges:
    | { changes: ChangeSet; selectionBefore: EditorSelection; textBefore: Text }
    | undefined

  const notifyErrorToast = useToast.error()
  const notifyError = notifyErrorToast.show.bind(notifyErrorToast)

  const debounceUpdates = createDebouncer(0)
  const updateListener: Extension = EditorView.updateListener.of((update) => {
    for (const transaction of update.transactions) {
      if (transaction.docChanged && !transaction.annotation(synchronizedModule)) {
        pendingChanges =
          pendingChanges ?
            {
              ...pendingChanges,
              changes: pendingChanges.changes.compose(transaction.changes),
            }
          : {
              changes: transaction.changes,
              selectionBefore: transaction.startState.selection,
              textBefore: transaction.startState.doc,
            }
        // Defer the update until after pending events have been processed, so that if changes are arriving faster
        // than we would be able to apply them individually we coalesce them to keep up.
        debounceUpdates(commitPendingChanges)
      }
    }
  })

  /** Set the editor contents to the current module state, discarding any pending editor-initiated changes. */
  function resetView() {
    pendingChanges = undefined
    const viewText = editorView.state.doc.toString()
    const code = graphStore.moduleSource.text
    const changes = textChangeToEdits(viewText, code)
    console.info('Resetting the editor to the module code.', changes)
    editorView.dispatch({
      changes,
      annotations: synchronizedModule.of(true),
    })
  }

  /** Apply any pending changes to the currently-synchronized module, clearing the set of pending changes. */
  function commitPendingChanges() {
    if (!pendingChanges) return
    const { changes, selectionBefore, textBefore } = pendingChanges
    pendingChanges = undefined
    const edits = changeSetToTextEdits(changes)
    try {
      const editedModule = graphStore.startEdit()
      editedModule.applyTextEdits(edits, graphStore.viewModule)
      if (editedModule.root()?.code() === editorView.state.doc.toString()) {
        graphStore.commitEdit(editedModule, undefined, 'local:userAction:CodeEditor')
        return
      }
      console.error(`Unable to apply source code edit.`, {
        expected: editorView.state.doc.toString(),
        got: editedModule.root()?.code(),
      })
    } catch (error) {
      console.error(`Code Editor failed to modify module`, error)
    }
    notifyError('Unable to apply source code edit.')
    editorView.dispatch({
      changes: changes.invert(textBefore),
      selection: selectionBefore,
      annotations: synchronizedModule.of(true),
    })
    if (graphStore.moduleSource.text !== editorView.state.doc.toString()) {
      console.warn('Unexpected: Applying inverted edit did not yield original module source')
      resetView()
    }
  }

  function beforeSourceChange({ origin }: { origin: Origin | undefined }) {
    if (pendingChanges && origin !== 'local:userAction:CodeEditor') commitPendingChanges()
  }

  function observeSourceChange(textEdits: readonly SourceRangeEdit[], origin: Origin | undefined) {
    if (origin !== 'local:userAction:CodeEditor') {
      editorView.dispatch({
        changes: textEdits,
        annotations: synchronizedModule.of(true),
      })
    }
  }

  /**
   * Starts synchronizing the editor with module updates. This must be called (once) *after* installing the
   * `updateListener` extension in the editor.
   */
  function connectModuleListener() {
    let cleanup: (() => void) | undefined = undefined
    watch(
      () => projectStore.module,
      (module, _oldValue, onCleanup) => {
        if (!module) return
        const beforeEditHandler = graphStore.onBeforeEdit(beforeSourceChange)
        graphStore.moduleSource.observe(observeSourceChange)
        cleanup = () => {
          beforeEditHandler?.unregister()
          graphStore.moduleSource.unobserve(observeSourceChange)
          cleanup = undefined
        }
        onCleanup(cleanup)
      },
      { immediate: true },
    )
    onUnmounted(() => cleanup?.())
  }

  return {
    /** The extension to install in the editor. */
    updateListener,
    connectModuleListener,
  }
}
