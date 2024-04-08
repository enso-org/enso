<script setup lang="ts">
import ExtendedMenu from '@/components/ExtendedMenu.vue'
import NavBar from '@/components/NavBar.vue'
import type { BreadcrumbItem } from '@/components/NavBreadcrumbs.vue'
import RecordControl from '@/components/RecordControl.vue'
import { injectGuiConfig } from '@/providers/guiConfig'
import { computed } from 'vue'

const props = defineProps<{
  breadcrumbs: BreadcrumbItem[]
  recordMode: boolean
  allowNavigationLeft: boolean
  allowNavigationRight: boolean
  zoomLevel: number
}>()
const emit = defineEmits<{
  recordOnce: []
  back: []
  forward: []
  breadcrumbClick: [index: number]
  'update:recordMode': [enabled: boolean]
  fitToAllClicked: []
  zoomIn: []
  zoomOut: []
  toggleCodeEditor: []
}>()

const config = injectGuiConfig()

const barStyle = computed(() => {
  const margin = config.value.window.topBar.margin
  const leftOffset = config.value.window.topBar.leftOffset
  return {
    marginLeft: `${leftOffset}px`,
    width: `calc(100% - ${leftOffset + margin * 2}px)`,
    top: `${margin}px`,
    left: `${margin}px`,
  }
})
</script>

<template>
  <div class="TopBar" :style="barStyle">
    <RecordControl
      :recordMode="props.recordMode"
      @update:recordMode="emit('update:recordMode', $event)"
      @recordOnce="emit('recordOnce')"
    />
    <NavBar
      :breadcrumbs="props.breadcrumbs"
      :allowNavigationLeft="props.allowNavigationLeft"
      :allowNavigationRight="props.allowNavigationRight"
      @back="emit('back')"
      @forward="emit('forward')"
      @breadcrumbClick="emit('breadcrumbClick', $event)"
    />
    <ExtendedMenu
      :zoomLevel="props.zoomLevel"
      @fitToAllClicked="emit('fitToAllClicked')"
      @zoomIn="emit('zoomIn')"
      @zoomOut="emit('zoomOut')"
      @toggleCodeEditor="emit('toggleCodeEditor')"
    />
  </div>
</template>

<style scoped>
.TopBar {
  position: absolute;
  display: flex;
  gap: 8px;
}
</style>
