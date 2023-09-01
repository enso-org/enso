<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import ProjectTitle, { type ProjectTitleModel } from './ProjectTitle.vue'
import { ref, type Ref } from 'vue'

const props = defineProps<{ breadcrumbs: string[] }>()
const emit = defineEmits<{ 'breadcrumbClick': [index: number] }>()

const title: Ref<ProjectTitleModel> = ref({ mode: "design" })
</script>

<template>
  <div class="TopBar">
    <ProjectTitle title="Test Project" v-model="title" />
    <NavBar :breadcrumbs="breadcrumbs" @breadcrumb-click="emit('breadcrumbClick', $event)" />
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
