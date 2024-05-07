<script setup lang="ts">
import ExtendedMenu from '@/components/ExtendedMenu.vue'
import NavBar from '@/components/NavBar.vue'
import type { BreadcrumbItem } from '@/components/NavBreadcrumbs.vue'
import RecordControl from '@/components/RecordControl.vue'
import SelectionMenu from '@/components/SelectionMenu.vue'
import { injectGuiConfig } from '@/providers/guiConfig'
import { computed } from 'vue'

const showColorPicker = defineModel<boolean>('showColorPicker', { required: true })
const props = defineProps<{
  breadcrumbs: BreadcrumbItem[]
  recordMode: boolean
  allowNavigationLeft: boolean
  allowNavigationRight: boolean
  zoomLevel: number
  componentsSelected: number
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
  collapseNodes: []
  setNodeColor: [color: string]
  removeNodes: []
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
    <Transition name="selection-menu">
      <SelectionMenu
        v-if="componentsSelected > 1"
        v-model:showColorPicker="showColorPicker"
        :selectedComponents="componentsSelected"
        @collapseNodes="emit('collapseNodes')"
        @removeNodes="emit('removeNodes')"
        @setNodeColor="emit('setNodeColor', $event)"
      />
    </Transition>
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
  top: 9px;
  /* FIXME[sb]: Get correct offset from dashboard. */
  left: 9px;
  width: 100%;
}

.selection-menu-enter-active,
.selection-menu-leave-active {
  transition: opacity 0.25s ease;
}

.selection-menu-enter-from,
.selection-menu-leave-to {
  opacity: 0;
}
</style>
