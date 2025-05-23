<script setup lang="ts">
import type { DocumentationMetadata } from '@/components/DocumentationPanel/metadata'
import DocsTag from '@/components/MarkdownEditor/DocsTag.vue'
import { computed, ref, watch } from 'vue'

const { metadata } = defineProps<{
  metadata: DocumentationMetadata
}>()

interface Tag {
  label: string
  value: string
  class?: string
  style?: { [key: string]: string | number | undefined }
}

const tags = computed<Tag[]>(() => {
  return [
    ...(metadata.group ?
      [
        {
          label: metadata.group.toLowerCase().replace('standard.base.', ''),
          value: '',
          style: {
            'background-color': 'var(--enso-docs-group-color)',
            color: '#fff',
          },
        },
      ]
    : []),
    ...(metadata.unstable ?
      [
        {
          label: 'unstable',
          value: '',
          style: {
            'background-color': '#e85252',
            color: '#fff',
          },
        },
      ]
    : []),
    ...(metadata.advanced ?
      [
        {
          label: 'advanced',
          value: '',
          style: {
            'background-color': '#e89d51',
            color: '#fff',
          },
        },
      ]
    : []),
    ...(metadata.deprecated ?
      [
        {
          label: 'deprecated',
          value: '',
          style: {
            'background-color': '#e89d51',
            color: '#fff',
          },
        },
      ]
    : []),
    ...(metadata.aliases ?? []).map((value) => ({ label: value, value: '' })),
    ...(metadata.private ? [{ label: 'private', value: '' }] : []),
    ...(metadata.added ? [{ label: 'added', value: metadata.added }] : []),
    ...(metadata.modified ? [{ label: 'modified', value: metadata.modified }] : []),
    ...(metadata.removed ? [{ label: 'removed', value: metadata.removed }] : []),
    ...(metadata.upcoming ? [{ label: 'upcoming', value: metadata.upcoming }] : []),
    ...(metadata.macros ?? []).map(({ description }) => ({
      label: description,
      value: '',
    })),
  ]
})

// === Show/hide excess tags. ===

const containerRef = ref<HTMLDivElement>()
const parentOffset = computed(() => containerRef.value?.offsetTop ?? 0)

const hiddenTags = ref(0)
const tagWasHidden = () => (hiddenTags.value += 1)
const someTagsAreHidden = computed(() => hiddenTags.value > 0)

const showAllTags = ref(false)
const showEllipsis = computed(() => someTagsAreHidden.value && !showAllTags.value)
watch(
  () => tags,
  () => {
    showAllTags.value = false
    hiddenTags.value = 0
  },
)
</script>

<template>
  <div v-if="tags.length > 0" ref="containerRef" class="Tags">
    <template v-for="(tag, i) in tags" :key="i">
      <DocsTag
        class="Tag"
        v-bind="tag"
        :parentOffset="parentOffset"
        :forceShow="showAllTags"
        @hidden="tagWasHidden"
      />
    </template>
    <button v-if="showEllipsis" class="Tag button" @click="() => (showAllTags = true)">…</button>
  </div>
</template>

<style scoped>
.Tags {
  display: flex;
  flex-flow: row wrap;
  align-items: start;
  gap: 2px;
}

.Tag {
  height: 24px;
  color: rgba(0, 0, 0, 0.6);
  background-color: var(--enso-docs-tag-background-color);
  border-radius: 4px;
  padding: 1px 5px;
}
</style>
