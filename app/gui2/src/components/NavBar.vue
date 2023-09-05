<script setup lang="ts">
import ArrowLeftIcon from '@/assets/icons/arrow_left.svg'
import ArrowRightIcon from '@/assets/icons/arrow_right.svg'
import GraphEditorIcon from '@/assets/icons/graph_editor.svg'

import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'

const props = defineProps<{ breadcrumbs: string[] }>()
const emit = defineEmits<{ previous: [], next: [], breadcrumbClick: [index: number] }>()
</script>

<template>
  <div class="NavBar">
    <img :src="GraphEditorIcon" draggable="false" class="icon" />
    <div class="breadcrumbs-controls">
      <img :src="ArrowLeftIcon" draggable="false" class="icon button" @click="emit('previous')" />
      <img :src="ArrowRightIcon" draggable="false" class="icon button" @click="emit('next')" />
    </div>
    <NavBreadcrumbs :breadcrumbs="breadcrumbs" @click="emit('breadcrumbClick', $event)" />
  </div>
</template>

<style scoped>
.NavBar {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;
}

.NavBar>.breadcrumbs-controls {
  display: flex;
}
</style>
