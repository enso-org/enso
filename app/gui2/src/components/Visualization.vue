<script setup lang="ts">
import FullscreenIcon from '@/assets/icons/fullscreen.svg'
import CompassIcon from '@/assets/icons/compass.svg'

const props = defineProps<{
  isFullscreen: boolean
  isVisualizationDropdownVisible: boolean
  data: {} | null
}>()
const emit = defineEmits<{
  preprocessorChange: [module: string, method: string, ...args: unknown[]]
  'update:isFullscreen': [isFullscreen: boolean]
}>()
</script>

<template>
  <div class="Visualization">
    <slot name="visualization"></slot>
    <div class="toolbars">
      <div class="toolbar">
        <button class="button active" @click="emit('update:isFullscreen', !isFullscreen)">
          <img :src="FullscreenIcon" />
        </button>
        <button class="button active"><img :src="CompassIcon" /></button>
      </div>
      <div class="toolbar">
        <slot name="toolbar"></slot>
      </div>
    </div>
    <div v-if="isVisualizationDropdownVisible"></div>
  </div>
</template>

<style scoped>
.toolbars {
  position: absolute;
  display: flex;
  gap: 4px;
  margin-top: 4px;
  top: 100%;
  left: 36px;
}

.toolbar {
  display: flex;
  background: white;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
}

.button {
  cursor: pointer;
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
}

.button.active {
  opacity: unset;
}

.button > * {
  vertical-align: top;
}
</style>
