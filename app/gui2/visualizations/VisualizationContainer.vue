<script setup lang="ts">
import FullscreenIcon from './Visualization/fullscreen.svg'
import ExitFullscreenIcon from './Visualization/exit_fullscreen.svg'
import EyeIcon from './Visualization/eye.svg'
import CompassIcon from './Visualization/compass.svg'

import VisualizationSelector from './Visualization/VisualizationSelector.vue'

import { ref } from 'vue'

// FIXME: resizers to change `width` and `height`
const props = defineProps<{
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
  /** Possible visualization types that can be switched to. */
  types: string[]
  background?: string
  isCircularMenuVisible: boolean
  width: number | undefined
  height: number | undefined
  fullscreen: boolean
  data?: {} | string
}>()
const emit = defineEmits<{
  hide: []
  'update:type': [type: string]
  'update:width': [width: number]
  'update:height': [height: number]
  'update:fullscreen': [fullscreen: boolean]
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const isSelectorVisible = ref(false)
</script>

<template>
  <Teleport to="body" :disabled="!fullscreen">
    <div
      class="Visualization"
      :class="{ fullscreen: fullscreen, 'below-node': belowNode, 'below-toolbar': belowToolbar }"
      :style="{ '--color-visualization-bg': background, width, height }"
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
            <img class="icon" :src="fullscreen ? ExitFullscreenIcon : FullscreenIcon" />
          </button>
          <div class="icon-container">
            <button class="button active" @click="isSelectorVisible = !isSelectorVisible">
              <img class="icon" :src="CompassIcon" />
            </button>
            <VisualizationSelector
              v-if="isSelectorVisible"
              :types="types"
              @update:type="
                (type) => {
                  isSelectorVisible = false
                  emit('update:type', type)
                }
              "
            />
          </div>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <slot name="toolbar"></slot>
        </div>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.Visualization {
  background: var(--color-visualization-bg);
  position: absolute;
  top: 50%;
  width: 100%;
  z-index: -1;
  border-radius: 16px;

  &.below-node {
    padding-top: 20px;
  }

  &.below-toolbar {
    padding-top: 56px;
  }

  &.fullscreen {
    z-index: var(--z-fullscreen);
    position: fixed;
    padding-top: 0;
    border-radius: 0;
    left: 0;
    top: 0;
    width: 100vw;
    height: 100vh;
  }

  &.fullscreen.below-toolbar {
    padding-top: 38px;
  }
}

.toolbars {
  user-select: none;
  position: absolute;
  display: flex;
  gap: 4px;
  top: 24px;
}

.Visualization.fullscreen .toolbars {
  top: 4px;
}

.toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;

  > * {
    position: relative;
  }

  > .background {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    border-radius: var(--radius-full);
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }

  &:not(:first-child):not(:has(> :nth-child(2))) {
    display: none;
  }
}

.button {
  cursor: pointer;
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;

  &.active {
    opacity: unset;
  }

  > * {
    vertical-align: top;
  }
}

.invisible {
  opacity: 0;
}

.hidden {
  display: none;
}

.icon-container {
  display: inline-flex;
}
</style>
