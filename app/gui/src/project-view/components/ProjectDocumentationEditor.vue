<script setup lang="ts">
import { BackendType, ProjectId } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { Ast } from '@/util/ast'
import { parseModule } from '@/util/ast/abstract'
import { useYTextSync } from '@/util/codemirror'
import { Err, Ok, unwrapOr } from '@/util/data/result'
import { EditorView } from '@codemirror/view'
import { useQuery } from '@tanstack/vue-query'
import { computed } from 'vue'
import MarkdownEditor from './MarkdownEditor.vue'

const props = defineProps<{
  id: ProjectId
  backendType: BackendType
}>()

const { backendForType } = useBackends()
const backendForAsset = computed(() => backendForType(props.backendType))

const fileContentsFromCloud = useQuery({
  queryKey: [
    props.backendType,
    {
      method: 'getFileContent',
      projectId: props.id,
    },
  ] as const,
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value.getFileContent(projectId)
  },
})

const content = computed(() => {
  switch (fileContentsFromCloud.status.value) {
    case 'pending':
      return Err('Loading documentation...')
    case 'error':
      return Err(fileContentsFromCloud.error.value)
    case 'success': {
      const code = fileContentsFromCloud.data.value
      if (code) {
        const module = parseModule(code)
        const statement = Ast.findModuleMethod(module, 'main')?.statement
        if (statement) return Ok(statement.mutableDocumentationMarkdown())
      }
    }
  }
  return Err('No documentation available')
})

const syncExt = (view: EditorView) => {
  const { syncExt, connectSync } = useYTextSync(() => unwrapOr(content.value, undefined))
  connectSync(view)
  return syncExt
}
</script>

<template>
  <MarkdownEditor :extensions="syncExt" readonly contentTestId="documentation-editor-content" />
</template>
