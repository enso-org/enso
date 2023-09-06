<script setup lang="ts">
import FullscreenIcon from './Visualization/fullscreen.svg'
import ExitFullscreenIcon from './Visualization/exit_fullscreen.svg'
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

const isFullscreen = ref(false)
const isChooserVisible = ref(false)
</script>

<template>
  <Teleport to="body" :disabled="!isFullscreen">
    <div
      class="Visualization"
      :class="{ fullscreen: isFullscreen, 'dodge-buttons': dodgeButtons }"
      :style="{ background: background ?? '#fff2f2' }"
    >
      <slot></slot>
      <div class="toolbars">
        <div v-if="!isCircularMenuVisible || isFullscreen"></div>
        <div :class="{ toolbar: true, invisible: isCircularMenuVisible, hidden: isFullscreen }">
          <div class="background"></div>
          <button class="button active" @click="emit('hide')"><img :src="EyeIcon" /></button>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <button class="button active" @click="isFullscreen = !isFullscreen">
            <img :src="isFullscreen ? ExitFullscreenIcon : FullscreenIcon" />
          </button>
          <button class="button active" @click="isChooserVisible = !isChooserVisible">
            <img :src="CompassIcon" />
          </button>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <slot name="toolbar"></slot>
        </div>
      </div>
      <div v-if="isChooserVisible"></div>
    </div>
  </Teleport>
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

.Visualization.fullscreen {
  z-index: var(--z-fullscreen);
  position: fixed;
  padding-top: 0;
  border-radius: 0;
  left: 0;
  top: 0;
  width: 100vw;
  height: 100vh;
}

.Visualization.fullscreen.dodge-buttons {
  padding-top: 38px;
}

.toolbars {
  position: absolute;
  display: flex;
  gap: 4px;
  top: 24px;
}

.Visualization.fullscreen .toolbars {
  top: 4px;
}

.toolbar > * {
  position: relative;
}

.toolbar > .background {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  border-radius: var(--radius-full);
  background: rgba(255, 255, 255, 80%);
  backdrop-filter: blur(64px);
}

.toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
}

.toolbar:not(:first-child):not(:has(> :nth-child(2))) {
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

.invisible {
  opacity: 0;
}

.hidden {
  display: none;
}
</style>
