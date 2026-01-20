<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import { useDocumentViewId } from '@/components/DocumentationEditor/documentViewId'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { Ast } from '@/util/ast'
import { parseModule } from '@/util/ast/abstract'
import { useYTextSync } from '@/util/codemirror'
import { editorPersistence } from '@/util/codemirror/persistence'
import { ResultComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { Err, mapOk, Ok, type Result, unwrapOr } from 'enso-common/src/utilities/data/result'
import { computed } from 'vue'

const rightPanel = useRightPanelData()
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
  enabled: computed(() => backendForAsset.value != null && projectId.value != null),
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value?.getMainFileContent(projectId)
  },
})

const currentMethodAst = computed((): Result<Ast.FunctionDef> => {
  if (fileContentsFromCloud.data != null) {
    if (fileContentsFromCloud.error.value) return Err(fileContentsFromCloud.error.value)
    if (fileContentsFromCloud.isLoading.value) return Err('Loading documentation...')
    const code = fileContentsFromCloud.data.value
    if (code) {
      const module = parseModule(code)
      const statement = Ast.findModuleMethod(module, 'main')?.statement
      if (statement) return Ok(statement)
    }
  }
  return Err('No documentation available')
})

const editorMarkdown = computed(() =>
  mapOk(currentMethodAst.value, (ast) => ast.mutableDocumentationMarkdown()),
)
const editorContent = computed(() => unwrapOr(editorMarkdown.value, undefined))

const { syncExt, connectSync } = useYTextSync(editorContent, 'local:userAction:DocEditor')
const editorPersistenceExt = editorPersistence({
  documentViewId: useDocumentViewId({
    projectId,
    methodPointer: undefined,
    view: 'DocumentationEditor',
  }),
  scroll: { y: true, x: false },
})

const extensions = [syncExt, editorPersistenceExt]
</script>

<template>
  <MarkdownEditor
    v-if="editorMarkdown.ok"
    :extensions="extensions"
    :readonly="true"
    contentTestId="documentation-editor-content"
    scrollerTestId="documentation-editor-scroller"
    :editorReadyCallback="connectSync"
  >
  </MarkdownEditor>
  <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
  <ResultComponent
    v-else
    status="info"
    :title="editorMarkdown.error.message('')"
    :centered="true"
  />
</template>
