<script setup lang="ts">
import type { DocumentationMetadata } from '@/components/ComponentHelp/metadata'
import DocsTag from '@/components/MarkdownEditor/DocsTag.vue'
import { useResizeObserver } from '@/composables/events'
import { computed, ref, useTemplateRef } from 'vue'

const { metadata } = defineProps<{
  metadata: DocumentationMetadata
}>()

interface Tag {
  label: string
  value?: string
  class?: string
}

function simple(value: string | undefined, label: string): Tag[] {
  if (value == null) return []
  return [{ label, value }]
}

function labelOnly(
  value: any | undefined,
  label: string | ((s: string) => string),
  className?: string | true,
): Tag[] {
  if (value == null) return []
  if (typeof label === 'function') label = label(value)
  return [
    {
      label,
      ...(className === true ? { class: label }
      : className != null ? { class: className }
      : {}),
    },
  ]
}

function groupLabel(group: string): string {
  return group.toLowerCase().replace('standard.base.', '')
}

const tags = computed<Tag[]>(() => {
  return [
    ...labelOnly(metadata.group, groupLabel, 'group'),
    ...labelOnly(metadata.unstable, 'unstable', true),
    ...labelOnly(metadata.advanced, 'advanced', true),
    ...labelOnly(metadata.deprecated, 'deprecated', true),
    ...labelOnly(metadata.private, 'private'),
    ...(metadata.aliases ?? []).map((label) => ({ label })),
    ...(metadata.macros ?? []).map(({ description: label }) => ({ label })),
    ...simple(metadata.added, 'added'),
    ...simple(metadata.modified, 'modified'),
    ...simple(metadata.removed, 'removed'),
    ...simple(metadata.upcoming, 'upcoming'),
  ]
})

// === Show/hide excess tags. ===
const showAll = ref(false)

/** A height of one line of tags. */
const LINE_HEIGHT = 24

const containerRef = useTemplateRef('containerRef')
const size = useResizeObserver(containerRef)
const hasOverflow = computed(() => {
  if (containerRef.value == null) return false
  const _track = size.value
  return containerRef.value.scrollHeight > LINE_HEIGHT
})
</script>

<template>
  <div v-if="tags.length > 0" class="FrontMatter" :style="{ '--line-height': `${LINE_HEIGHT}px` }">
    <div ref="containerRef" :class="{ Tags: true, showAll }">
      <template v-for="(tag, i) in tags" :key="i">
        <DocsTag v-bind="tag" />
      </template>
    </div>
    <button v-if="hasOverflow" class="ellipsisButton" @click="() => (showAll = !showAll)">…</button>
  </div>
</template>

<style scoped>
.FrontMatter {
  display: flex;
  gap: 2px;
  align-items: end;
}

.Tags {
  display: flex;
  flex-shrink: 1;
  flex-flow: row wrap;
  overflow: hidden;
  max-height: var(--line-height);
  align-items: start;
  gap: 2px;
  position: relative;
  transition: max-height ease-in-out 0.2s;

  &.showAll {
    flex-wrap: wrap;
    overflow: visible;
    max-height: 500px;
  }
}

.ellipsisButton {
  flex: 0 0 auto;
  height: var(--line-height);
  color: rgba(0, 0, 0, 0.6);
  background-color: #dcd8d8;
  border-radius: 4px;
  padding: 1px 5px;
}
</style>
