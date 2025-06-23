<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { backendMutationOptions } from '@/composables/backend'
import { useStringSync } from '@/util/codemirror'
import { ResultComponent } from '@/util/react'
import { EditorView } from '@codemirror/view'
import { useMutation } from '@tanstack/vue-query'
import { computed, Ref, watch } from 'vue'

const rightPanel = useRightPanelData()
const { backendForType } = useBackends()
const backendForAsset = computed(
  () =>
    (rightPanel.context?.category && backendForType(rightPanel.context.category.backend)) ?? null,
)

// Provide an extra `mutationKey` so that it has its own loading state.
const editDescriptionMutation = useMutation(
  backendMutationOptions('updateAsset', backendForAsset, { mutationKey: ['editDescription'] }),
)

const syncText = (view: EditorView, focused: Ref<boolean>) => {
  const { syncExt, connectSync } = useStringSync()
  const { setText, getText } = connectSync(view)
  watch(
    () => rightPanel.focusedAsset?.description,
    (content) => {
      setText(content ?? '')
    },
    { immediate: true },
  )
  console.log('Attach extension')
  watch(focused, (newVal) => {
    console.log('Focus Change', newVal)
    if (!newVal && rightPanel.focusedAsset) {
      editDescriptionMutation.mutate([
        rightPanel.focusedAsset.id,
        { parentDirectoryId: null, description: getText(), title: null },
        rightPanel.focusedAsset.title,
      ])
    }
  })

  return syncExt
}

provideDocumentationImages({
  openedProject: () => null,
  backend: backendForAsset,
  projectId: null,
})
</script>

<template>
  <MarkdownEditor
    v-if="rightPanel.focusedAsset"
    :extensions="syncText"
    contentTestId="documentation-editor-content"
  />
  <ResultComponent
    v-else
    status="info"
    title="Select single asset to edit its description"
    :centered="true"
  />
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
