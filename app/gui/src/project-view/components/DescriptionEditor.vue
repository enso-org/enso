<script setup lang="ts">
import { AnyAsset } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { backendMutationOptions } from '@/composables/backend'
import { useStringSync } from '@/util/codemirror'
import { ResultComponent } from '@/util/react'
import { EditorView } from '@codemirror/view'
import { useMutation } from '@tanstack/vue-query'
import { computed, effectScope, onScopeDispose, ref, watch } from 'vue'

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

function updateDescription(asset: AnyAsset | undefined, description: string) {
  if (asset != null && asset.description !== description) {
    editDescriptionMutation.mutate([
      asset.id,
      { parentDirectoryId: null, description: description, title: null },
      asset.title,
    ])
  }
}

const scope = effectScope()
const onFocusOut = ref<() => void>()

const syncText = (view: EditorView) => {
  const { syncExt, setText, getText } = useStringSync(view)

  scope.run(() => {
    watch(
      () => rightPanel.focusedAsset,
      (newAsset, oldAsset) => {
        updateDescription(oldAsset, getText())
        const pendingDescription =
          newAsset != null && editDescriptionMutation.variables.value?.[0] === newAsset.id ?
            editDescriptionMutation.variables.value[1].description
          : undefined

        setText(pendingDescription ?? newAsset?.description ?? '')
      },
      { immediate: true },
    )

    onFocusOut.value = () => {
      updateDescription(rightPanel.focusedAsset, getText())
    }

    onScopeDispose(() => updateDescription(rightPanel.focusedAsset, getText()))
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
  <div class="DescriptionEditor">
    <MarkdownEditor
      v-if="rightPanel.focusedAsset"
      :extensions="syncText"
      toolbar
      contentTestId="documentation-editor-content"
    />
    <ResultComponent
      v-else
      status="info"
      title="Select single asset to edit its description"
      :centered="true"
    />
  </div>
</template>

<style scoped>
.DescriptionEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
  padding-left: 4px;
  padding-right: 4px;
}
</style>
