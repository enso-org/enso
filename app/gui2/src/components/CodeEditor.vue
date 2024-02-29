<script setup lang="ts">
import type { ChangeSet, Diagnostic, Highlighter } from '@/components/CodeEditor/codemirror'
import { usePointer } from '@/composables/events'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { useAutoBlur } from '@/util/autoBlur'
import { chain } from '@/util/data/iterable'
import { unwrap } from '@/util/data/result'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { useLocalStorage } from '@vueuse/core'
import { createDebouncer } from 'lib0/eventloop'
import { MutableModule } from 'shared/ast'
import { textChangeToEdits, type SourceRangeEdit } from 'shared/util/data/text'
import { rangeEncloses, type Origin } from 'shared/yjsModel'
import { computed, onMounted, onUnmounted, ref, shallowRef, watch, watchEffect } from 'vue'

// Use dynamic imports to aid code splitting. The codemirror dependency is quite large.
const {
  Annotation,
  StateEffect,
  StateField,
  bracketMatching,
  foldGutter,
  lintGutter,
  highlightSelectionMatches,
  minimalSetup,
  EditorState,
  EditorView,
  syntaxHighlighting,
  defaultHighlightStyle,
  tooltips,
  enso,
  linter,
  forceLinting,
  lsDiagnosticsToCMDiagnostics,
  hoverTooltip,
} = await import('@/components/CodeEditor/codemirror')

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const suggestionDbStore = useSuggestionDbStore()
const rootElement = ref<HTMLElement>()
useAutoBlur(rootElement)

const executionContextDiagnostics = shallowRef<Diagnostic[]>([])

// Effect that can be applied to the document to invalidate the linter state.
const diagnosticsUpdated = StateEffect.define()
// State value that is perturbed by any `diagnosticsUpdated` effect.
const diagnosticsVersion = StateField.define({
  create: (_state) => 0,
  update: (value, transaction) => {
    for (const effect of transaction.effects) {
      if (effect.is(diagnosticsUpdated)) value += 1
    }
    return value
  },
})

const expressionUpdatesDiagnostics = computed(() => {
  const updates = projectStore.computedValueRegistry.db
  const panics = updates.type.reverseLookup('Panic')
  const errors = updates.type.reverseLookup('DataflowError')
  const diagnostics: Diagnostic[] = []
  for (const externalId of chain(panics, errors)) {
    const update = updates.get(externalId)
    if (!update) continue
    const astId = graphStore.db.idFromExternal(externalId)
    if (!astId) continue
    const span = graphStore.moduleSource.getSpan(astId)
    if (!span) continue
    const [from, to] = span
    switch (update.payload.type) {
      case 'Panic': {
        diagnostics.push({ from, to, message: update.payload.message, severity: 'error' })
        break
      }
      case 'DataflowError': {
        const error = projectStore.dataflowErrors.lookup(externalId)
        if (error?.value?.message) {
          diagnostics.push({ from, to, message: error.value.message, severity: 'error' })
        }
        break
      }
    }
  }
  return diagnostics
})

// == CodeMirror editor setup  ==

const editorView = new EditorView()
const viewInitialized = ref(false)
watchEffect(() => {
  const module = projectStore.module
  if (!module) return
  editorView.setState(
    EditorState.create({
      extensions: [
        minimalSetup,
        updateListener(),
        diagnosticsVersion,
        syntaxHighlighting(defaultHighlightStyle as Highlighter),
        bracketMatching(),
        foldGutter(),
        lintGutter(),
        highlightSelectionMatches(),
        tooltips({ position: 'absolute' }),
        hoverTooltip((ast, syn) => {
          const dom = document.createElement('div')
          const astSpan = ast.span()
          let foundNode: NodeId | undefined
          for (const [id, node] of graphStore.db.nodeIdToNode.entries()) {
            const rootSpan = graphStore.moduleSource.getSpan(node.rootSpan.id)
            if (rootSpan && rangeEncloses(rootSpan, astSpan)) {
              foundNode = id
              break
            }
          }
          const expressionInfo = foundNode && graphStore.db.getExpressionInfo(foundNode)
          const nodeColor = foundNode && graphStore.db.getNodeColorStyle(foundNode)

          if (foundNode != null) {
            dom
              .appendChild(document.createElement('div'))
              .appendChild(document.createTextNode(`AST ID: ${foundNode}`))
          }
          if (expressionInfo != null) {
            dom
              .appendChild(document.createElement('div'))
              .appendChild(document.createTextNode(`Type: ${expressionInfo.typename ?? 'Unknown'}`))
          }
          if (expressionInfo?.profilingInfo[0] != null) {
            const profile = expressionInfo.profilingInfo[0]
            const executionTime = (profile.ExecutionTime.nanoTime / 1_000_000).toFixed(3)
            const text = `Execution Time: ${executionTime}ms`
            dom
              .appendChild(document.createElement('div'))
              .appendChild(document.createTextNode(text))
          }

          dom
            .appendChild(document.createElement('div'))
            .appendChild(document.createTextNode(`Syntax: ${syn.toString()}`))
          const method = expressionInfo?.methodCall?.methodPointer
          if (method != null) {
            const moduleName = tryQualifiedName(method.module)
            const methodName = tryQualifiedName(method.name)
            const qualifiedName = qnJoin(unwrap(moduleName), unwrap(methodName))
            const [id] = suggestionDbStore.entries.nameToId.lookup(qualifiedName)
            const suggestionEntry = id != null ? suggestionDbStore.entries.get(id) : undefined
            if (suggestionEntry != null) {
              const groupNode = dom.appendChild(document.createElement('div'))
              groupNode.appendChild(document.createTextNode('Group: '))
              const groupNameNode = groupNode.appendChild(document.createElement('span'))
              groupNameNode.appendChild(document.createTextNode(`${method.module}.${method.name}`))
              if (nodeColor) {
                groupNameNode.style.color = nodeColor
              }
            }
          }
          return { dom }
        }),
        enso(),
        linter(
          () => [...executionContextDiagnostics.value, ...expressionUpdatesDiagnostics.value],
          {
            needsRefresh(update) {
              return (
                update.state.field(diagnosticsVersion) !==
                update.startState.field(diagnosticsVersion)
              )
            },
          },
        ),
      ],
    }),
  )
  viewInitialized.value = true
})

function changeSetToTextEdits(changes: ChangeSet) {
  const textEdits = new Array<SourceRangeEdit>()
  changes.iterChanges((from, to, _fromB, _toB, insert) =>
    textEdits.push({ range: [from, to], insert: insert.toString() }),
  )
  return textEdits
}
function textEditToChangeSpec({ range: [from, to], insert }: SourceRangeEdit) {
  return { from, to, insert }
}

let pendingChanges: ChangeSet | undefined
let currentModule: MutableModule | undefined
/** Set the editor contents the current module state, discarding any pending editor-initiated changes. */
function resetView() {
  console.info(`Resetting the editor to the module code.`)
  pendingChanges = undefined
  currentModule = undefined
  const viewText = editorView.state.doc.toString()
  const code = graphStore.moduleSource.text
  editorView.dispatch({
    changes: textChangeToEdits(viewText, code).map(textEditToChangeSpec),
    annotations: synchronizedModule.of(graphStore.startEdit()),
  })
}

/** Apply any pending changes to the currently-synchronized module, clearing the set of pending changes. */
function commitPendingChanges() {
  if (!pendingChanges || !currentModule) return
  try {
    currentModule.applyTextEdits(changeSetToTextEdits(pendingChanges), graphStore.viewModule())
    graphStore.commitEdit(currentModule, undefined, 'local:CodeEditor')
  } catch (error) {
    console.error(`Code Editor failed to modify module`, error)
    resetView()
  }
  pendingChanges = undefined
}

function updateListener() {
  const debouncer = createDebouncer(0)
  return EditorView.updateListener.of((update) => {
    for (const transaction of update.transactions) {
      const newModule = transaction.annotation(synchronizedModule)
      if (newModule) {
        // Flush the pipeline of edits that were based on the old module.
        commitPendingChanges()
        currentModule = newModule
      } else if (transaction.docChanged && currentModule) {
        pendingChanges = pendingChanges
          ? pendingChanges.compose(transaction.changes)
          : transaction.changes
        // Defer the update until after pending events have been processed, so that if changes are arriving faster than
        // we would be able to apply them individually we coalesce them to keep up.
        debouncer(commitPendingChanges)
      }
    }
  })
}

let needResync = false
// Indicates a change updating the text to correspond to the given module state.
const synchronizedModule = Annotation.define<MutableModule>()
watch(
  viewInitialized,
  (ready) => {
    if (ready) graphStore.moduleSource.observe(observeSourceChange)
  },
  { immediate: true },
)
onUnmounted(() => graphStore.moduleSource.unobserve(observeSourceChange))

function observeSourceChange(textEdits: SourceRangeEdit[], origin: Origin | undefined) {
  // If we received an update from outside the Code Editor while the editor contained uncommitted changes, we cannot
  // proceed incrementally; we wait for the changes to be merged as Y.Js AST updates, and then set the view to the
  // resulting code.
  if (needResync) {
    if (!pendingChanges) {
      resetView()
      needResync = false
    }
    return
  }
  // When we aren't in the `needResync` state, we can ignore updates that originated in the Code Editor.
  if (origin === 'local:CodeEditor') return
  if (pendingChanges) {
    console.info(`Deferring update (editor dirty).`)
    needResync = true
    return
  }

  // If none of the above exit-conditions were reached, the transaction is applicable to our current state.
  editorView.dispatch({
    changes: textEdits.map(textEditToChangeSpec),
    annotations: synchronizedModule.of(graphStore.startEdit()),
  })
}

// The LS protocol doesn't identify what version of the file updates are in reference to. When diagnostics are received
// from the LS, we map them to the text assuming that they are applicable to the current version of the module. This
// will be correct if there is no one else editing, and we aren't editing faster than the LS can send updates. Typing
// too quickly can result in incorrect ranges, but at idle it should correct itself when we receive new diagnostics.
watch([viewInitialized, () => projectStore.diagnostics], ([ready, diagnostics]) => {
  if (!ready) return
  executionContextDiagnostics.value = graphStore.moduleSource.text
    ? lsDiagnosticsToCMDiagnostics(graphStore.moduleSource.text, diagnostics)
    : []
})

watch([executionContextDiagnostics, expressionUpdatesDiagnostics], () => {
  editorView.dispatch({ effects: diagnosticsUpdated.of(null) })
  forceLinting(editorView)
})

onMounted(() => {
  editorView.focus()
  rootElement.value?.prepend(editorView.dom)
})

const editorSize = useLocalStorage<{ width: number | null; height: number | null }>(
  'code-editor-size',
  { width: null, height: null },
)

let initSize = { width: 0, height: 0 }
const resize = usePointer((pos, _, type) => {
  if (rootElement.value == null) return
  if (type == 'start') initSize = rootElement.value.getBoundingClientRect()
  editorSize.value.width = initSize.width + pos.relative.x
  editorSize.value.height = initSize.height - pos.relative.y
})

function resetSize() {
  editorSize.value.width = null
  editorSize.value.height = null
}

const editorStyle = computed(() => {
  return {
    width: editorSize.value.width ? `${editorSize.value.width}px` : '50%',
    height: editorSize.value.height ? `${editorSize.value.height}px` : '30%',
  }
})
</script>

<template>
  <div
    ref="rootElement"
    class="CodeEditor"
    :style="editorStyle"
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @wheel.stop.passive
    @pointerdown.stop
    @pointerup.stop
    @click.stop
    @contextmenu.stop
  >
    <div class="resize-handle" v-on="resize.events" @dblclick="resetSize">
      <svg viewBox="0 0 16 16">
        <circle cx="2" cy="2" r="1.5" />
        <circle cx="8" cy="2" r="1.5" />
        <circle cx="8" cy="8" r="1.5" />
        <circle cx="14" cy="2" r="1.5" />
        <circle cx="14" cy="8" r="1.5" />
        <circle cx="14" cy="14" r="1.5" />
      </svg>
    </div>
  </div>
</template>

<style scoped>
.CodeEditor {
  position: absolute;
  bottom: 5px;
  left: 5px;
  width: 50%;
  height: 30%;
  max-width: calc(100% - 10px);
  max-height: calc(100% - 10px);
  backdrop-filter: var(--blur-app-bg);
  border-radius: 7px;
  font-family: var(--font-mono);

  &.v-enter-active,
  &.v-leave-active {
    transition:
      transform 0.2s ease,
      opacity 0.2s ease;
  }

  &.v-enter-from,
  &.v-leave-to {
    transform: scale(95%);
    opacity: 0;
  }
}

:deep(.ͼ1 .cm-scroller) {
  font-family: var(--font-mono);
}

.resize-handle {
  position: absolute;
  top: -3px;
  right: -3px;
  width: 20px;
  height: 20px;
  padding: 5px;
  cursor: nesw-resize;

  svg {
    fill: black;
    width: 100%;
    height: 100%;
    opacity: 0.1;
    transition: opacity 0.1s ease-in-out;
  }

  &:hover svg {
    opacity: 0.9;
  }
}

.CodeEditor :is(.cm-editor) {
  position: relative;
  color: white;
  width: 100%;
  height: 100%;
  background-color: rgba(255, 255, 255, 0.35);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.4);
  border-radius: 5px;
  opacity: 1;
  color: black;
  text-shadow: 0 0 2px rgba(255, 255, 255, 0.4);
  font-size: 12px;
  outline: 1px solid transparent;
  transition: outline 0.1s ease-in-out;
}

.CodeEditor :is(.cm-focused) {
  outline: 1px solid rgba(0, 0, 0, 0.5);
}

.CodeEditor :is(.cm-tooltip-hover) {
  padding: 4px;
  border-radius: 4px;
  border: 1px solid rgba(0, 0, 0, 0.4);
  text-shadow: 0 0 2px rgba(255, 255, 255, 0.4);

  &::before {
    content: '';
    background-color: rgba(255, 255, 255, 0.35);
    backdrop-filter: blur(64px);
    border-radius: 4px;
  }
}

.CodeEditor :is(.cm-gutters) {
  border-radius: 3px 0 0 3px;
}
</style>
