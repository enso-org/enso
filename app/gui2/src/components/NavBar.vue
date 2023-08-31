<script setup lang="ts">
import ArrowLeftIcon from '@/assets/icons/arrow_left.svg'
import ArrowRightIcon from '@/assets/icons/arrow_right.svg'
import GraphEditorIcon from '@/assets/icons/graph_editor.svg'

import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import type { Breadcrumb } from '@/components/NavBreadcrumb.vue'

defineProps<{ breadcrumbs: Breadcrumb[] }>()
</script>

<template>
  <div>
    <div class="blur-full"></div>
    <div class="NavBar">
      <img :src="GraphEditorIcon" draggable="false" class="icon" />
      <div class="breadcrumbs-controls">
        <img :src="ArrowLeftIcon" draggable="false" class="icon button" />
        <img :src="ArrowRightIcon" draggable="false" class="icon button" />
      </div>
      <NavBreadcrumbs :breadcrumbs="breadcrumbs" />
    </div>
  </div>
</template>

<style scoped>
.NavBar {
  display: flex;
  border-radius: var(--radius-full);
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
