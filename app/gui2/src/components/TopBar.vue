<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import ProjectTitle from '@/components/ProjectTitle.vue'

const props = defineProps<{ title: string; breadcrumbs: string[]; modes: string[]; mode: string }>()
const emit = defineEmits<{
  execute: []
  back: []
  forward: []
  breadcrumbClick: [index: number]
  'update:mode': [mode: string]
}>()
</script>

<template>
  <div class="TopBar">
    <ProjectTitle
      :title="title"
      :modes="modes"
      :mode="mode"
      @update:mode="emit('update:mode', $event)"
      @execute="emit('execute')"
    />
    <NavBar
      :breadcrumbs="breadcrumbs"
      @back="emit('back')"
      @forward="emit('forward')"
      @breadcrumbClick="emit('breadcrumbClick', $event)"
    />
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
