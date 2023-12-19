<script setup lang="ts">
import DocsTag from '@/components/DocumentationPanel/DocsTag.vue'
import type { Doc } from '@/util/docParser'
import { computed, ref, watch } from 'vue'

const props = defineProps<{ tags: Doc.Section.Tag[]; groupColor: string }>()

const skipTags: Doc.Tag[] = ['Icon', 'TextOnly']
const tags = computed<Doc.Section.Tag[]>(() => {
  return props.tags.flatMap((tag) => {
    if (tag.tag === 'Alias') {
      // Split Alias tags into separate ones.
      return tag.body.split(/,\s*/).map((body) => ({ tag: 'Alias', body }))
    } else if (skipTags.includes(tag.tag)) {
      // Skip certain tags.
      return []
    } else {
      return tag
    }
  })
})

// === Sorting ===

const weight: Partial<Record<Doc.Tag, number>> = {
  Group: 1000,
  Unstable: 502,
  Advanced: 501,
  Deprecated: 500,
  Alias: 100,
}

const sortedTags = computed<Doc.Section.Tag[]>(() => {
  return [...tags.value].sort((a, b) => (weight[b.tag] ?? 0) - (weight[a.tag] ?? 0))
})

// === View ===

export interface View {
  label: string
  class?: string
  style?: { [key: string]: string | number | undefined }
}

const defaultView = (tag: Doc.Tag, body: Doc.HtmlString): View => ({
  label: (tag + (body.length > 0 ? `: ${body}` : '')).toLowerCase(),
})
const tagsMap: Partial<Record<Doc.Tag, (body: Doc.HtmlString) => View>> = {
  Advanced: (_body) => ({
    label: 'advanced',
    style: {
      'background-color': '#e89d51',
      color: '#fff',
    },
  }),
  Alias: (body) => ({ label: body }),
  Deprecated: (_body) => ({
    label: 'deprecated',
    style: {
      'background-color': '#e89d51',
      color: '#fff',
    },
  }),
  Group: (body) => ({
    label: body.toLowerCase().replace('standard.base.', ''),
    style: {
      'background-color': props.groupColor,
      color: '#fff',
    },
  }),
  Unstable: (_body) => ({
    label: 'unstable',
    style: {
      'background-color': '#e85252',
      color: '#fff',
    },
  }),
}
const view = (tag: Doc.Section.Tag): View => {
  const maybeView = tagsMap[tag.tag]
  return maybeView ? maybeView(tag.body) : defaultView(tag.tag, tag.body)
}
const views = computed<View[]>(() => sortedTags.value.map(view))

// === Show/hide excess tags. ===

const containerRef = ref<HTMLDivElement>()
const parentOffset = computed(() => containerRef.value?.offsetTop ?? 0)

const hiddenTags = ref(0)
const tagWasHidden = () => (hiddenTags.value += 1)
const someTagsAreHidden = computed(() => hiddenTags.value > 0)

const showAllTags = ref(false)
const showEllipsis = computed(() => someTagsAreHidden.value && !showAllTags.value)
watch(
  () => props.tags,
  () => {
    showAllTags.value = false
    hiddenTags.value = 0
  },
)
</script>

<template>
  <div v-if="views.length > 0" ref="containerRef" class="Tags">
    <template v-for="(v, i) in views" :key="i">
      <DocsTag
        class="Tag"
        :view="v"
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
  background-color: var(--enso-docs-tag-background-color);
  border-radius: 4px;
  padding: 1px 5px;
}
</style>
