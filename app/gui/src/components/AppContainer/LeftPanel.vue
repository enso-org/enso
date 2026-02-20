<script setup lang="ts">
import { useContainerData } from '$/providers/container'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, toRef, useTemplateRef } from 'vue'
import { Drive } from './reactTabs'

const containerData = useContainerData()
const width = toRef(containerData, 'leftPanelWidth')

const root = useTemplateRef('root')
const size = useResizeObserver(root)
const bounds = computed(() => new Rect(Vec2.Zero, size.value))
const cssClass = computed(() => ({
  focusedPanel: containerData.focusedPanel.type === 'drive',
}))
const style = computed(() => (width.value == null ? {} : { width: `${width.value}px` }))
</script>

<template>
  <SizeTransition width :duration="250">
    <div ref="root" class="LeftPanel" :class="cssClass" :style="style">
      <Drive />
      <div class="shadow" />
      <ResizeHandles right :modelValue="bounds" @update:modelValue="width = $event.width" />
    </div>
  </SizeTransition>
</template>

<style scoped>
.LeftPanel {
  flex-shrink: 1;
  flex-grow: 1;
  height: 100%;
  position: relative;
  z-index: 1;
}

.shadow {
  position: absolute;
  top: 0;
  right: 0;
  width: 100%;
  height: 100%;
  box-shadow:
    0.5px 2.2px 0px rgb(0 0 0 / 0.84%),
    0 1.2px 5.65px 0px rgb(0 0 0 / 1.21%),
    0 2.25px 10.64px 0 rgb(0 0 0 / 1.5%),
    0 4px 19px 0 rgb(0 0 0 / 1.79%),
    0 7.5px 35.5px 0 rgb(0 0 0 / 2.16%),
    0 18px 85px 0 rgb(0 0 0 / 3%);
  /* clip-path: polygon(-100vw 0, 100% 0, 100% 100%, -100vw 100%); */
  z-index: -1;
}
</style>
