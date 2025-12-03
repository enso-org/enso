<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import StandaloneButton from '@/components/StandaloneButton.vue'
import { ResultComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { fileExtension } from 'enso-common/src/utilities/file'
import { computed, ref, watch, watchEffect } from 'vue'

const rightPanel = useRightPanelData()
const { backendForType } = useBackends()
const backendForAsset = computed(
  () =>
    (rightPanel.context?.category && backendForType(rightPanel.context.category.backend)) ?? null,
)
const isPreviewingMediaFile = ref(false)

const filePath = computed(() =>
  typeof rightPanel.context?.item === 'object' && rightPanel.context.item.type === 'file' ?
    rightPanel.context.item.ensoPath
  : undefined,
)

watch(filePath, () => {
  isPreviewingMediaFile.value = false
})

const fileType = computed(() => {
  if (!filePath.value) {
    return undefined
  }
  const extension = fileExtension(filePath.value)
  switch (extension?.toLowerCase()) {
    case 'png':
    case 'jpg':
    case 'jpeg':
    case 'gif':
    case 'webp': {
      return 'image'
    }
    case 'mp3':
    case 'wav': {
      return 'audio'
    }
    case 'mp4':
    case 'mov':
    case 'webm': {
      return 'video'
    }
    case 'txt':
    case 'json':
    case 'yaml':
    case 'csv':
    case 'js':
    case 'ts':
    case 'vue':
    case 'jsx':
    case 'tsx':
    case 'html': {
      return 'text'
    }
    default:
      return undefined
  }
})

const fileDetails = useQuery({
  queryKey: computed(() => {
    // Only preview text, image, audio and video files.
    if (fileType.value === undefined) {
      return []
    }
    if (fileType.value !== 'text' && !isPreviewingMediaFile.value) {
      // Media file preview not activated yet.
      return []
    }
    const fileId =
      typeof rightPanel.context?.item === 'object' && rightPanel.context.item.type === 'file' ?
        rightPanel.context.item.id
      : undefined
    if (!fileId) {
      return []
    }
    const title =
      typeof rightPanel.context?.item === 'object' ? rightPanel.context.item.title : '(unknown)'
    return [backendForAsset.value, 'getFileDetails', fileId, title, true] as const
  }),
  queryFn: async ({ queryKey: [backend, , fileId, title, fetchContents] }) => {
    if (!fileId) {
      return null
    }
    if (!backend) {
      throw new Error('No backend available for asset')
    }
    return await backend.getFileDetails(fileId, title, fetchContents)
  },
})

const fileUrl = computed(() => fileDetails.data?.value?.url)

const fileContentsQuery = useQuery({
  queryKey: computed(() => ['fetch', fileUrl.value, fileType.value] as const),
  queryFn: async ({ queryKey: [, url, fileType] }) => {
    if (!url || !fileType) {
      return null
    }
    const response = await fetch(url)
    if (!response.ok) {
      throw new Error(`Failed to fetch file contents: ${response.statusText}`)
    }
    return { type: fileType, response }
  },
})

const fileContents = ref<
  | null
  | { type: 'text'; url?: never; text?: string }
  | { type: 'image' | 'audio' | 'video'; url: string; text?: never }
>(null)

watchEffect(async (onCleanup) => {
  const value = fileContentsQuery.data?.value
  if (!value) {
    return
  }
  let newValue = fileContents.value
  switch (value.type) {
    case 'image':
    case 'audio':
    case 'video': {
      const blob = await value.response.blob()
      const url = URL.createObjectURL(blob)
      newValue = fileContents.value = { type: value.type, url }
      break
    }
    case 'text': {
      newValue = fileContents.value = { type: value.type, text: await value.response.text() }
      break
    }
  }
  onCleanup(() => {
    if (newValue && 'url' in newValue) {
      URL.revokeObjectURL(newValue.url)
    }
    fileContents.value = null
  })
})

const projectContentsQuery = useQuery({
  queryKey: computed(
    () => [backendForAsset.value, 'getMainFileContent', rightPanel.focusedProject] as const,
  ),
  queryFn: async ({ queryKey: [backend, , projectId] }) => {
    if (!projectId) {
      return null
    }
    if (!backend) {
      throw new Error('No backend available for asset')
    }
    const content = await backend.getMainFileContent(projectId)
    return content
  },
})

// Strip out metadata section from project contents.
// If this is ever intended to be editable, this logic should be removed completely.
const projectContents = computed(() => {
  const value = projectContentsQuery.data?.value
  if (!value) {
    return
  }
  return value.replace(/\n+#### METADATA ####[\s\S]+$/, '')
})
</script>

<template>
  <div class="AssetContentsEditor">
    <h2>File contents</h2>
    <div class="contents">
      <StandaloneButton
        v-if="
          (fileType === 'image' || fileType === 'audio' || fileType === 'video') &&
          !isPreviewingMediaFile
        "
        :label="`Click to preview ${fileType} file`"
        @activate="isPreviewingMediaFile = true"
      />
      <template v-else-if="fileContentsQuery.data.value">
        <p v-if="fileContentsQuery.data.value.type === 'text'">
          {{ fileContentsQuery.data }}
        </p>
        <img
          v-else-if="fileContents?.type === 'image'"
          :src="fileContents.url"
          alt="Image preview"
        />
        <audio v-else-if="fileContents?.type === 'audio'" :src="fileContents.url" controls>
          Your browser does not support the audio element.
        </audio>
        <video v-else-if="fileContents?.type === 'video'" :src="fileContents.url" controls>
          Your browser does not support the video element.
        </video>
      </template>
      <pre v-else-if="projectContents"><code>{{ projectContents }}</code></pre>
      <LoadingSpinner
        v-else-if="fileType || rightPanel.focusedProject"
        phase="loading-medium"
        :size="20"
      />
      <ResultComponent
        v-else-if="rightPanel.context?.item"
        status="info"
        title="No preview available for this asset"
        :centered="true"
      />
      <ResultComponent
        v-else
        status="info"
        title="Select a single asset to see its preview"
        :centered="true"
      />
    </div>
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

.contents > p,
.contents > pre {
  width: 100%;
  min-height: 100%;
}

code {
  white-space: pre-wrap;
  word-break: break-word;
}

.contents {
  display: grid;
  place-items: center;
  height: 100%;
}
</style>
