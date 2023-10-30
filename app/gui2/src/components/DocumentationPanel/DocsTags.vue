<script setup lang="ts">
import type { Doc } from '@/util/docParser'
import { computed } from 'vue'

const props = defineProps<{ tags: Doc.Section.Tag[] }>()

// Split Alias tags into separate ones.
const tags = computed<Doc.Section.Tag[]>(() => {
  return props.tags.flatMap((tag) => {
    if (tag.tag === 'Alias') {
      return tag.body.split(/,\s*/).map((body) => ({ tag: 'Alias', body }))
    } else {
      return tag
    }
  })
})

interface View {
  label: string
  class?: string
  style?: { [key: string]: string | number | undefined }
}

const defaultView = (tag: Doc.Tag, body: Doc.HtmlString): View => ({
  label: tag + (body.length > 0 ? `=${body}` : ''),
})
const tagsMap: Partial<Record<Doc.Tag, (body: Doc.HtmlString) => View>> = {
  Alias: (body) => ({ label: body }),
}
const view = (tag: Doc.Section.Tag): View => {
  const maybeView = tagsMap[tag.tag]
  return maybeView ? maybeView(tag.body) : defaultView(tag.tag, tag.body)
}
</script>

<template>
  <div v-if="tags.length > 0" class="Tags">
    <div
      v-for="(tag, index) in tags"
      :key="index"
      :class="view(tag).class || 'tag'"
      :style="view(tag).style || {}"
    >
      {{ view(tag).label }}
    </div>
  </div>
</template>

<style scoped>
.Tags {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  align-items: start;
  gap: 2px;
}

.tag {
  display: flex;
  height: 24px;
  align-items: flex-start;
  background-color: var(--enso-docs-tag-background-color);
  border-radius: 4px;
  padding: 1px 5px;
}
</style>
