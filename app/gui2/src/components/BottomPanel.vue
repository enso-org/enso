<script setup lang="ts">
import { codeEditorBindings } from '@/bindings'
import ResizeHandles from '@/components/ResizeHandles.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { useLocalStorage } from '@vueuse/core'
import { computed, ref } from 'vue'

const MIN_DOCK_SIZE_PX = 20

const rootElement = ref<HTMLElement>()

const show = defineModel<boolean>('show', { required: true })

const savedSize = useLocalStorage<{ height: number | null }>('code-editor-size', { height: null })

const computedSize = useResizeObserver(rootElement)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

function clampSize(size: number) {
  return Math.max(size, MIN_DOCK_SIZE_PX)
}

const style = computed(() =>
  savedSize.value?.height != null ?
    { '--panel-size': `${clampSize(savedSize.value.height)}px` }
  : undefined,
)
</script>

<template>
  <ToggleIcon
    v-model="show"
    :title="`Code Editor (${codeEditorBindings.bindings.toggle.humanReadable})`"
    icon="bottom_panel"
    class="toggleDock"
  />
  <Transition>
    <div v-if="show" ref="rootElement" class="BottomPanel dock" :style="style">
      <slot />
      <ResizeHandles
        top
        :modelValue="computedBounds"
        @update:modelValue="savedSize = { height: $event.height }"
      />
    </div>
  </Transition>
</template>

<style scoped>
.BottomPanel {
  --panel-size: var(--code-editor-default-height);
  position: relative;
  bottom: 0;
  height: var(--panel-size);
  margin-right: 1px;
  backdrop-filter: var(--blur-app-bg);
  background-color: rgba(255, 255, 255, 0.35);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.4);
}
.v-enter-active,
.v-leave-active {
  transition: height 0.1s ease;
}
.v-enter-from,
.v-leave-to {
  height: 0;
}

.toggleDock {
  z-index: 1;
  position: absolute;
  left: 12px;
  bottom: 12px;
}
</style>
