<script setup lang="ts">
import FullscreenIcon from './Visualization/fullscreen.svg'
import ExitFullscreenIcon from './Visualization/exit_fullscreen.svg'
import EyeIcon from './Visualization/eye.svg'
import CompassIcon from './Visualization/compass.svg'

import { ref } from 'vue'

// FIXME: visualization chooser
// FIXME: resizers to change `width` and `height`
const props = defineProps<{
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
  background?: string
  isCircularMenuVisible: boolean
  width: number | undefined
  height: number | undefined
  fullscreen: boolean
  data: {} | string
}>()
const emit = defineEmits<{
  hide: []
  'update:width': [width: number]
  'update:height': [height: number]
  'update:fullscreen': [fullscreen: boolean]
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const isChooserVisible = ref(false)
</script>

<template>
  <Teleport to="body" :disabled="!fullscreen">
    <div
      class="Visualization"
      :class="{ fullscreen: fullscreen, 'below-node': belowNode, 'below-toolbar': belowToolbar }"
      :style="{ background: background ?? '#fff2f2', width, height }"
    >
      <slot></slot>
      <div class="toolbars">
        <div v-if="!isCircularMenuVisible || fullscreen"></div>
        <div :class="{ toolbar: true, invisible: isCircularMenuVisible, hidden: fullscreen }">
          <div class="background"></div>
          <button class="button active" @click="emit('hide')"><img :src="EyeIcon" /></button>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <button class="button active" @click="emit('update:fullscreen', !fullscreen)">
            <img :src="fullscreen ? ExitFullscreenIcon : FullscreenIcon" />
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
  width: 100%;
  z-index: -1;
  border-radius: 16px;
}

.Visualization.below-node {
  padding-top: 20px;
}

.Visualization.below-toolbar {
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

.Visualization.fullscreen.below-toolbar {
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
