<script setup lang="ts">
import ArrowLeftIcon from '@/assets/icons/arrow_left.svg'
import ArrowRightIcon from '@/assets/icons/arrow_right.svg'
import GraphEditorIcon from '@/assets/icons/graph_editor.svg'

import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import type { Breadcrumb } from '@/components/NavBreadcrumb.vue'

const props = defineProps<{
  breadcrumbs: Breadcrumb[]
}>()
</script>

<template>
  <div class="NavBar">
    <div class="blur-container"></div>
    <div class="contents">
      <img :src="GraphEditorIcon" class="icon" />
      <div class="breadcrumbs-controls">
        <img :src="ArrowLeftIcon" class="icon" />
        <img :src="ArrowRightIcon" class="icon" />
      </div>
      <NavBreadcrumbs :breadcrumbs="breadcrumbs" />
    </div>
  </div>
</template>

<style scoped>
.NavBar {
  position: relative;
  border-radius: var(--radius-full);
}

.NavBar>* {
  position: relative;
}

.NavBar>.blur-container {
  position: absolute;
  width: 100%;
  height: 100%;
  border-radius: var(--radius-full);
  background-color: var(--color-frame-bg);
  backdrop-filter: var(--backdrop-blur);
}

.NavBar>.contents {
  display: flex;
  border-radius: var(--radius-full);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;
}

.NavBar>.contents>.breadcrumbs-controls {
  display: flex;
}
</style>
