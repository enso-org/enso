<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import type { Breadcrumb } from '@/components/NavBreadcrumb.vue'
import ProjectTitle, { type ProjectTitleModel } from './ProjectTitle.vue'
import { ref, type Ref } from 'vue'

defineProps<{ breadcrumbs: Breadcrumb[] }>()

const title: Ref<ProjectTitleModel> = ref({ mode: "design" })
</script>

<template>
  <div class="TopBar">
    <ProjectTitle title="Test Project" v-model="title" />
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
