<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import type { Breadcrumb } from '@/components/NavBreadcrumb.vue'
import ProjectTitle from './ProjectTitle.vue'

const props = defineProps<{ breadcrumbs: Breadcrumb[] }>()
</script>

<template>
  <div class="TopBar">
    <ProjectTitle title="Test Project" mode="design" />
    <NavBar :breadcrumbs="breadcrumbs" />
  </div>
</template>

<style scoped>
.TopBar {
  position: relative;
  display: flex;
  gap: 8px;
  top: 9px;
  /* FIXME[sb]: Get correct offset from dashboard. */
  left: 9px;
}
</style>
