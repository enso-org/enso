<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import ProjectTitle from '@/components/ProjectTitle.vue'
import { injectGuiConfig } from '@/providers/guiConfig'
import { computed } from 'vue'

const props = defineProps<{ title: string; breadcrumbs: string[]; modes: string[]; mode: string }>()
const emit = defineEmits<{
  execute: []
  back: []
  forward: []
  breadcrumbClick: [index: number]
  'update:mode': [mode: string]
}>()

const LEFT_PADDING_PX = 11

const config = injectGuiConfig()

const barStyle = computed(() => {
  const offset = Number(config.value.window?.topBarOffset ?? '0')
  return {
    marginLeft: `${offset + LEFT_PADDING_PX}px`,
  }
})
</script>

<template>
  <div class="TopBar" :style="barStyle">
    <ProjectTitle
      :title="props.title"
      :modes="props.modes"
      :mode="props.mode"
      @update:mode="emit('update:mode', $event)"
      @execute="emit('execute')"
    />
    <NavBar
      :breadcrumbs="props.breadcrumbs"
      @back="emit('back')"
      @forward="emit('forward')"
      @breadcrumbClick="emit('breadcrumbClick', $event)"
    />
  </div>
</template>

<style scoped>
.TopBar {
  position: absolute;
  display: flex;
  gap: 8px;
  top: 9px;
  /* FIXME[sb]: Get correct offset from dashboard. */
  left: 9px;
}
</style>
