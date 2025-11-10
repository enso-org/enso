<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { useDocumentViewId } from '@/components/DocumentationEditor/documentViewId'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { useYTextSync } from '@/util/codemirror'
import { editorPersistence } from '@/util/codemirror/persistence'
import { methodPointerEquals } from '@/util/methodPointer'
import { ResultComponent } from '@/util/react'
import { mapOk, unwrapOr } from 'enso-common/src/utilities/data/result'
import { computed } from 'vue'

const { store: project, graph } = useCurrentProject()
const projectId = computed(() => project.value.id)

const currentMethodPointer = computed(() => unwrapOr(graph.value.currentMethod.pointer, undefined))
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    !methodPointerEquals(currentMethodPointer.value, project.value.entryPoint),
)

const editorMarkdown = computed(() =>
  mapOk(graph.value.currentMethod.ast, (ast) => ast.mutableDocumentationMarkdown()),
)
const editorContent = computed(() => unwrapOr(editorMarkdown.value, undefined))

const { syncExt, connectSync } = useYTextSync(editorContent, 'local:userAction:DocEditor')
const editorPersistenceExt = editorPersistence({
  documentViewId: useDocumentViewId({
    projectId,
    methodPointer: currentMethodPointer,
    view: 'DocumentationEditor',
  }),
  scroll: { y: true, x: false },
})

const extensions = [syncExt, editorPersistenceExt]
</script>

<template>
  <MarkdownEditor
    v-if="graph.currentMethod.ast.ok"
    :extensions="extensions"
    contentTestId="documentation-editor-content"
    scrollerTestId="documentation-editor-scroller"
    :editorReadyCallback="connectSync"
  >
    <template #belowToolbar>
      <FunctionSignatureEditor
        v-if="displaySignatureEditor"
        :projectId="projectId"
        :functionAst="graph.currentMethod.ast.value"
        :methodPointer="currentMethodPointer"
      />
    </template>
  </MarkdownEditor>
  <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
  <ResultComponent
    v-else
    status="info"
    :title="graph.currentMethod.ast.error.message('')"
    :centered="true"
  />
</template>
