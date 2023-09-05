<script setup lang="ts">
import EyeIcon from './Visualization/eye.svg'
import CompassIcon from './Visualization/compass.svg'
import { ref } from 'vue'

// FIXME: add back `width` and `height` to parent components
const props = defineProps<{
  /** If true, the visualization should have extra padding to avoid buttons as well. */
  dodgeButtons?: boolean
  background?: string
  isCircularMenuVisible: boolean
}>()
const emit = defineEmits<{
  hide: []
}>()

const isChooserVisible = ref(false)
</script>

<template>
  <div
    class="Visualization"
    :class="{ 'dodge-buttons': dodgeButtons }"
    :style="{ background: background ?? '#fff2f2' }"
  >
    <slot></slot>
    <div class="toolbars">
      <div v-if="!isCircularMenuVisible"></div>
      <div :class="{ toolbar: true, invisible: isCircularMenuVisible }">
        <button class="button active" @click="emit('hide')"><img :src="EyeIcon" /></button>
      </div>
      <div class="toolbar">
        <button class="button active" @click="isChooserVisible = !isChooserVisible">
          <img :src="CompassIcon" />
        </button>
      </div>
      <div class="toolbar">
        <slot name="toolbar"></slot>
      </div>
    </div>
    <div v-if="isChooserVisible"></div>
  </div>
</template>

<style scoped>
.Visualization {
  position: absolute;
  top: 50%;
  padding-top: 16px;
  width: 100%;
  z-index: -1;
  border-bottom-left-radius: 16px;
  border-bottom-right-radius: 16px;
}

.Visualization.dodge-buttons {
  padding-top: 52px;
}

.toolbars {
  position: absolute;
  display: flex;
  gap: 4px;
  top: 20px;
}

.toolbar {
  display: flex;
  background: rgba(255, 255, 255, 80%);
  backdrop-filter: blur(64px);
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
}

.toolbar:not(:first-child):not(:has(> *)) {
  display: none;
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
