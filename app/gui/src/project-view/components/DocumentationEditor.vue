<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { Ast } from '@/util/ast'
import { parseModule } from '@/util/ast/abstract'
import { useYTextSync } from '@/util/codemirror'
import { Err, mapOk, Ok, unwrapOr } from '@/util/data/result'
import { methodPointerEquals } from '@/util/methodPointer'
import { ResultComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { computed } from 'vue'

const rightPanel = useRightPanelData()
const openedProject = useCurrentProject().ref
const projectId = computed(() => rightPanel.focusedProject)
const { backendForType } = useBackends()
const backendForAsset = computed(() => {
  if (rightPanel.context?.category == null) return null
  return backendForType(rightPanel.context.category.backend)
})

const fileContentsFromCloud = useQuery({
  queryKey: computed(
    () =>
      [
        backendForAsset.value?.type,
        {
          method: 'getFileContent',
          projectId: projectId.value,
        },
      ] as const,
  ),
  enabled: computed(
    () => openedProject.value == null && backendForAsset.value != null && projectId.value != null,
  ),
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value?.getFileContent(projectId)
  },
})

const currentMethodAst = computed(() => {
  if (openedProject.value) {
    return mapOk(openedProject.value.graph.currentMethod.ast, (ast) => ({ ast, readOnly: false }))
  } else if (fileContentsFromCloud.data != null) {
    if (fileContentsFromCloud.error.value) return Err(fileContentsFromCloud.error.value)
    if (fileContentsFromCloud.isLoading.value) return Err('Loading documentation...')
    const code = fileContentsFromCloud.data.value
    if (code) {
      const module = parseModule(code)
      const statement = Ast.findModuleMethod(module, 'main')?.statement
      if (statement) return Ok({ ast: statement, readOnly: true })
    }
  }
  return Err('No documentation available')
})

const currentMethodPointer = computed(
  () => openedProject.value && unwrapOr(openedProject.value.graph.currentMethod.pointer, undefined),
)
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    openedProject.value?.store.entryPoint &&
    !methodPointerEquals(currentMethodPointer.value, openedProject.value.store.entryPoint),
)

const editorMarkdown = computed(() =>
  mapOk(currentMethodAst.value, ({ ast }) => ast.mutableDocumentationMarkdown()),
)
const editorContent = computed(() => unwrapOr(editorMarkdown.value, undefined))

const { syncExt, connectSync } = useYTextSync(editorContent)

provideDocumentationImages({
  openedProject,
  backend: backendForAsset,
  projectId,
})
</script>

<template>
  <div class="DocumentationEditor">
    <MarkdownEditor
      v-if="currentMethodAst.ok"
      :extensions="syncExt"
      :readonly="currentMethodAst.value.readOnly"
      contentTestId="documentation-editor-content"
      scrollerTestId="documentation-editor-scroller"
      @editorReady="connectSync"
    >
      <template #belowToolbar>
        <FunctionSignatureEditor
          v-if="displaySignatureEditor && currentMethodAst.ok && openedProject"
          :projectId="openedProject.store.id"
          :functionAst="currentMethodAst.value.ast"
          :methodPointer="currentMethodPointer"
        />
      </template>
    </MarkdownEditor>
    <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
    <ResultComponent
      v-else
      status="info"
      :title="currentMethodAst.error.message('')"
      :centered="true"
    />
  </div>
</template>

<style scoped>
.DocumentationEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
  padding-left: 4px;
  padding-right: 4px;
}
</style>
