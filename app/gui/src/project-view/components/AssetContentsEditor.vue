<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import { useQuery } from '@tanstack/vue-query'
import { computed } from 'vue'

const rightPanel = useRightPanelData()
const { backendForType } = useBackends()
const backendForAsset = computed(
  () =>
    (rightPanel.context?.category && backendForType(rightPanel.context.category.backend)) ?? null,
)

const fileDetails = useQuery({
  queryKey: computed(() => {
    const filePath =
      typeof rightPanel.context?.item === 'object' && rightPanel.context.item.type === 'file' ?
        rightPanel.context.item.ensoPath
      : undefined
    // Only preview text files.
    if (!/[.](?:txt|json|yaml|csv)$/.test(filePath ?? '')) {
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
    return ['getFileDetails', fileId, title, true] as const
  }),
  queryFn: async ({ queryKey: [, fileId, title, fetchContents] }) => {
    if (!fileId) {
      return null
    }
    const backend = backendForAsset.value
    if (!backend) {
      throw new Error('No backend available for asset')
    }
    const fileDetails = await backend.getFileDetails(fileId, title, fetchContents)
    return fileDetails
  },
})

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

const projectContentsQuery = useQuery({
  queryKey: computed(() => {
    const projectId =
      typeof rightPanel.context?.item === 'object' ?
        rightPanel.context.item.type === 'project' ?
          rightPanel.context.item.id
        : undefined
      : rightPanel.context?.item
    if (!projectId) {
      return []
    }
    return ['getMainFileContent', projectId] as const
  }),
  queryFn: async ({ queryKey: [, projectId] }) => {
    if (!projectId) {
      return null
    }
    const backend = backendForAsset.value
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
    <p v-if="fileContentsQuery.data.value">
      {{ fileContentsQuery.data }}
    </p>
    <pre v-else-if="projectContents"><code>{{ projectContents }}</code></pre>
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

code {
  white-space: pre-wrap;
  word-break: break-word;
}
</style>
