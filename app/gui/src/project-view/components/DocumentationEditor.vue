<script setup lang="ts">
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { Ast } from '@/util/ast'
import { parseModule } from '@/util/ast/abstract'
import { Err, mapOk, Ok, unwrapOr } from '@/util/data/result'
import { methodPointerEquals } from '@/util/methodPointer'
import { ResultComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { computed } from 'vue'

const rightPanel = useRightPanelData()
const currentProject = injectCurrentProject()
const projectId = computed(() => currentProject.id.value ?? rightPanel.focusedProject)
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
    () =>
      currentProject.ref.value == null && backendForAsset.value != null && projectId.value != null,
  ),
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value?.getMainFileContent(projectId)
  },
})

const currentMethodAst = computed(() => {
  if (currentProject.ref.value) {
    return mapOk(currentProject.ref.value.graph.currentMethod.ast, (ast) => ({
      ast,
      readOnly: false,
    }))
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
  () =>
    currentProject.ref.value &&
    unwrapOr(currentProject.ref.value.graph.currentMethod.pointer, undefined),
)
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    currentProject.ref.value?.store.entryPoint &&
    !methodPointerEquals(currentMethodPointer.value, currentProject.ref.value.store.entryPoint),
)

const editorMarkdown = computed(() =>
  mapOk(currentMethodAst.value, ({ ast, readOnly }) => {
    const docs = ast.mutableDocumentationMarkdown()
    return readOnly ? docs.toString() : docs
  }),
)

provideDocumentationImages({
  openedProject: currentProject.ref,
  backend: backendForAsset,
  projectId,
})
</script>

<template>
  <div class="DocumentationEditor">
    <MarkdownEditor
      v-if="editorMarkdown.ok"
      :content="editorMarkdown.value"
      contentTestId="documentation-editor-content"
    >
      <template #belowToolbar>
        <FunctionSignatureEditor
          v-if="displaySignatureEditor && currentMethodAst.ok"
          :projectId="projectId"
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
      :title="editorMarkdown.error.message('')"
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
