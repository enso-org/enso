<script setup lang="ts">
import { AnyAsset } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { backendMutationOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { useStringSync } from '@/util/codemirror'
import { ResultComponent } from '@/util/react'
import { EditorView } from '@codemirror/view'
import { useMutation } from '@tanstack/vue-query'
import { isOnElectron } from 'enso-common/src/detect'
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
function updateDescription(asset: AnyAsset | undefined, description: string) {
  if (asset != null && asset.description !== description) {
    descriptionEdited = false
    return editDescriptionMutation.mutateAsync([
      asset.id,
      { parentDirectoryId: null, description: description, title: null },
      asset.title,
    ])
  } else {
    return Promise.resolve()
  }
}

const onFocusOut = ref<() => void>()
const { syncExt, connectSync } = useStringSync()
const scope = effectScope()

function onEditorReady(view: EditorView) {
  const { setText, getText, onTextEdited } = connectSync(view)

  // We want to run watch before DOM update, because the DescriptionEditor may be disposed as
  // part of it. Therefore it must be in the DescriptionEditor effect socope, not MarkdownEditor.
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

    onTextEdited(() => (descriptionEdited = true))

    onFocusOut.value = () => {
      updateDescription(rightPanel.focusedAsset, getText())
    }

    onScopeDispose(() => updateDescription(rightPanel.focusedAsset, getText()))

    useEvent(window, 'beforeunload', (event) => {
      if (descriptionEdited) {
        event.preventDefault()
        // While browser displays "unsaved changes" warining, electron does nothing for
        // preventDefault. That gives us a chance to save changes and close manually.
        if (isOnElectron()) {
          updateDescription(rightPanel.focusedAsset, getText()).then(() => window.close())
        }
      }
    })
  })
}
</script>

<template>
  <div class="DescriptionEditor">
    <MarkdownEditor
      v-if="rightPanel.focusedAsset"
      :extensions="syncExt"
      contentTestId="asset-panel-description"
      @editorReady="onEditorReady"
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
