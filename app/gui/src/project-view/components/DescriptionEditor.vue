<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { backendMutationOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { useStringSync } from '@/util/codemirror'
import { ResultComponent } from '@/util/react'
import { EditorView } from '@codemirror/view'
import { useMutation } from '@tanstack/vue-query'
import type { AssetDetailsResponse, AssetId } from 'enso-common/src/services/Backend'
import { isOnElectron } from 'enso-common/src/utilities/detect'
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

let descriptionEdited = false
async function updateDescription(
  asset: AssetDetailsResponse<AssetId> | undefined,
  description: string,
) {
  descriptionEdited = false
  if (asset && description && asset.description !== description) {
    await editDescriptionMutation.mutateAsync([
      asset.id,
      {
        parentDirectoryId: null,
        description: description,
        title: null,
        metadataId: asset.metadataId,
      },
      asset.title,
    ])
  }
}

const onFocusOut = ref<() => void>()
const { syncExt, setText, getText } = useStringSync({
  onTextEdited: () => (descriptionEdited = true),
})
const scope = effectScope()

function editorReadyCallback(view: EditorView) {
  // We want to run watch before DOM update, because the DescriptionEditor may be disposed as
  // part of it. Therefore it must be in the DescriptionEditor effect scope, not MarkdownEditor.
  scope.run(() => {
    watch(
      () => rightPanel.focusedAssetDetails,
      (newAsset, oldAsset) => {
        updateDescription(oldAsset, getText(view))
        const pendingDescription =
          newAsset != null && editDescriptionMutation.variables.value?.[0] === newAsset.id ?
            editDescriptionMutation.variables.value[1].description
          : undefined

        setText(view, pendingDescription ?? newAsset?.description ?? '')
      },
      { immediate: true },
    )

    onFocusOut.value = () => {
      updateDescription(rightPanel.focusedAssetDetails, getText(view))
    }

    onScopeDispose(() => updateDescription(rightPanel.focusedAssetDetails, getText(view)))

    useEvent(window, 'beforeunload', (event) => {
      if (descriptionEdited) {
        event.preventDefault()
        // While browser displays "unsaved changes" warning, electron does nothing for
        // preventDefault. That gives us a chance to save changes and close manually.
        if (isOnElectron()) {
          updateDescription(rightPanel.focusedAssetDetails, getText(view)).then(() =>
            window.close(),
          )
        }
      }
    })
  })
}
</script>

<template>
  <div class="DescriptionEditor">
    <MarkdownEditor
      v-if="rightPanel.focusedAssetDetails"
      :extensions="syncExt"
      contentTestId="asset-panel-description"
      :editorReadyCallback="editorReadyCallback"
    />
    <ResultComponent
      v-else
      status="info"
      title="Select a single asset to edit its description"
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
