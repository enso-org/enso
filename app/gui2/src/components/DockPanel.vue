<script setup lang="ts">
import { documentationEditorBindings } from '@/bindings'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'

const MIN_DOCK_SIZE_PX = 200

const toolbarElement = ref<HTMLElement>()
const slideInPanel = ref<HTMLElement>()

const show = defineModel<boolean>('show', { required: true })
const size = defineModel<number | undefined>('size')

const computedSize = useResizeObserver(slideInPanel)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

function clampSize(size: number) {
  return Math.max(size, MIN_DOCK_SIZE_PX)
}

const style = computed(() => ({
  width: size.value != null ? `${clampSize(size.value)}px` : 'var(--right-dock-default-width)',
}))
</script>

<template>
  <ToggleIcon
    v-model="show"
    :title="`Documentation Panel (${documentationEditorBindings.bindings.toggle.humanReadable})`"
    icon="right_panel"
    class="toggleDock"
  />
  <SizeTransition width :duration="100">
    <div v-if="show" ref="slideInPanel" :style="style" class="DockPanel" data-testid="rightDock">
      <div ref="toolbarElement" class="toolbar"></div>
      <div class="scrollArea">
        <slot :toolbar="toolbarElement" />
      </div>
      <ResizeHandles left :modelValue="computedBounds" @update:modelValue="size = $event.width" />
    </div>
  </SizeTransition>
</template>

<style scoped>
.DockPanel {
  background-color: rgb(255, 255, 255);
  position: relative;
}

.scrollArea {
  width: 100%;
  height: 100%;
  overflow-y: auto;
  padding-left: 10px;
  /* Prevent touchpad back gesture, which can be triggered while panning. */
  overscroll-behavior-x: none;
}

.toolbar {
  height: 48px;
  padding-left: 4px;
}

.toggleDock {
  z-index: 1;
  position: absolute;
  right: 16px;
  top: 16px;
}
</style>
