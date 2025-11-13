<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import { useUploadsToCloudStore } from '$/providers/upload'
import { backendQueryOptions } from '@/composables/backend'
import { useUploadLocally } from '@/util/upload'
import { useQuery } from '@tanstack/vue-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { computed } from 'vue'

const rightPanel = useRightPanelData()
const { backendForType } = useBackends()
const backendForAsset = computed(
  () =>
    (rightPanel.context?.category && backendForType(rightPanel.context.category.backend)) ?? null,
)

const fileDetails = useQuery(
  backendQueryOptions(
    'getFileDetails',
    computed(() => {
      const filePath =
        typeof rightPanel.context?.item === 'object' && rightPanel.context.item.type === 'file' ?
          rightPanel.context.item.ensoPath
        : undefined
      // Only preview text files.
      if (!/[.](?:txt|json|yaml|csv)$/.test(filePath ?? '')) {
        return
      }
      const fileId =
        typeof rightPanel.context?.item === 'object' && rightPanel.context.item.type === 'file' ?
          rightPanel.context.item.id
        : undefined
      if (!fileId) {
        return
      }
      const title =
        typeof rightPanel.context?.item === 'object' ? rightPanel.context.item.title : '(unknown)'
      return [fileId, title, true]
    }),
    backendForAsset.value,
  ),
)

const fileUrl = computed(() => fileDetails.data?.value?.url)

const fileContentsQuery = useQuery({
  queryKey: computed(() => ['fetch', fileUrl] as const),
  queryFn: async ({ queryKey: [, url] }) => {
    if (!url) {
      return null
    }
    const response = await fetch(url)
    if (!response.ok) {
      throw new Error(`Failed to fetch file contents: ${response.statusText}`)
    }
    return await response.text()
  },
})

const uploads = useUploadsToCloudStore()
const uploadLocally = useUploadLocally(backendForAsset)
const uploadFile = computed(() =>
  backendForAsset.value?.type === BackendType.local ?
    uploadLocally
  : uploads.uploadFile.bind(uploads),
)
</script>

<template>
  <div class="AssetContentsEditor">
    <h2>File contents</h2>
    <p>
      {{ fileContentsQuery.data }}
    </p>
  </div>
</template>

<style scoped>
.AssetContentsEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
  gap: var(--side-panel-section-gap);
}

h2 {
  font-size: 1.125rem;
  line-height: var(--snug-line-height);
}
</style>
