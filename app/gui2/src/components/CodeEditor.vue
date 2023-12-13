<script setup lang="ts">
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { useAutoBlur } from '@/util/autoBlur'
import type { Diagnostic, Highlighter } from '@/util/codemirror'
import { usePointer } from '@/util/events'
import { chain } from '@/util/iterable'
import { useLocalStorage } from '@vueuse/core'
import { rangeEncloses, type ExprId } from 'shared/yjsModel'
import { computed, onMounted, ref, watch, watchEffect } from 'vue'
import { qnJoin, tryQualifiedName } from '../util/qualifiedName'
import { unwrap } from '../util/result'

// Use dynamic imports to aid code splitting. The codemirror dependency is quite large.
const {
  bracketMatching,
  foldGutter,
  lintGutter,
  highlightSelectionMatches,
  minimalSetup,
  EditorState,
  EditorView,
  yCollab,
  syntaxHighlighting,
  defaultHighlightStyle,
  tooltips,
  enso,
  linter,
  forceLinting,
  lsDiagnosticsToCMDiagnostics,
  hoverTooltip,
} = await import('@/util/codemirror')

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const suggestionDbStore = useSuggestionDbStore()
const rootElement = ref<HTMLElement>()
useAutoBlur(rootElement)

const executionContextDiagnostics = computed(() =>
  projectStore.module
    ? lsDiagnosticsToCMDiagnostics(
        projectStore.module.doc.contents.toString(),
        projectStore.diagnostics,
      )
    : [],
)

const expressionUpdatesDiagnostics = computed(() => {
  const nodeMap = graphStore.db.nodeIdToNode
  const updates = projectStore.computedValueRegistry.db
  const panics = updates.type.reverseLookup('Panic')
  const errors = updates.type.reverseLookup('DataflowError')
  const diagnostics: Diagnostic[] = []
  for (const id of chain(panics, errors)) {
    const update = updates.get(id)
    if (!update) continue
    const node = nodeMap.get(id)
    if (!node) continue
    if (!node.rootSpan.astExtended) continue
    const [from, to] = node.rootSpan.astExtended.span()
    switch (update.payload.type) {
      case 'Panic': {
        diagnostics.push({ from, to, message: update.payload.message, severity: 'error' })
        break
      }
      case 'DataflowError': {
        const error = projectStore.dataflowErrors.lookup(id)
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
watchEffect(() => {
  const module = projectStore.module
  if (!module) return
  const yText = module.doc.contents
  const undoManager = module.undoManager
  const awareness = projectStore.awareness.internal
  editorView.setState(
    EditorState.create({
      doc: yText.toString(),
      extensions: [
        minimalSetup,
        yCollab(yText, awareness, { undoManager }),
        syntaxHighlighting(defaultHighlightStyle as Highlighter),
        bracketMatching(),
        foldGutter(),
        lintGutter(),
        highlightSelectionMatches(),
        tooltips({ position: 'absolute' }),
        hoverTooltip((ast, syn) => {
          const dom = document.createElement('div')
          const astSpan = ast.span()
          let foundNode: ExprId | undefined
          for (const [id, node] of graphStore.db.nodeIdToNode.entries()) {
            if (
              node.rootSpan.astExtended &&
              rangeEncloses(node.rootSpan.astExtended.span(), astSpan)
            ) {
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
        linter(() => [...executionContextDiagnostics.value, ...expressionUpdatesDiagnostics.value]),
      ],
    }),
  )
})

watch([executionContextDiagnostics, expressionUpdatesDiagnostics], () => forceLinting(editorView))

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
