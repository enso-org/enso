<script setup lang="ts">
import FullscreenIcon from './icons/fullscreen.svg'
import ExitFullscreenIcon from './icons/exit_fullscreen.svg'
import EyeIcon from './icons/eye.svg'
import CompassIcon from './icons/compass.svg'

import VisualizationSelector from './Visualization/VisualizationSelector.vue'

import { usePointer, PointerButtonMask } from './events.ts'
import type { Vec2 } from './builtins.ts'

import { ref } from 'vue'

// FIXME: resizers to change `width` and `height`
const props = defineProps<{
  /** If true, the visualization should be `overflow: visible` instead of `overflow: hidden`. */
  overflow?: boolean
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
  /** Possible visualization types that can be switched to. */
  types: string[]
  background?: string
  isCircularMenuVisible: boolean
  nodeSize: Vec2
  width: number | null
  height: number | null
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

function onWheel(event: WheelEvent) {
  if (
    event.currentTarget instanceof Element &&
    (event.currentTarget.scrollWidth > event.currentTarget.clientWidth ||
      event.currentTarget.scrollHeight > event.currentTarget.clientHeight)
  ) {
    event.stopPropagation()
  }
}

const rootNode = ref<HTMLElement>()
const contentNode = ref<HTMLElement>()

const resizeRight = usePointer((pos, _, type) => {
  if (type !== 'move' || pos.delta.x === 0) {
    return
  }
  const width = pos.absolute.x - (contentNode.value?.getBoundingClientRect().left ?? 0)
  emit('update:width', Math.max(props.nodeSize.x, width))
}, PointerButtonMask.Main)

const resizeBottom = usePointer((pos, _, type) => {
  if (type !== 'move' || pos.delta.y === 0) {
    return
  }
  const height = pos.absolute.y - (contentNode.value?.getBoundingClientRect().top ?? 0)
  emit('update:height', Math.max(0, height))
}, PointerButtonMask.Main)

const resizeBottomRight = usePointer((pos, _, type) => {
  if (type !== 'move') {
    return
  }
  if (pos.delta.x !== 0) {
    const width = pos.absolute.x - (contentNode.value?.getBoundingClientRect().left ?? 0)
    emit('update:width', Math.max(props.nodeSize.x, width))
  }
  if (pos.delta.y !== 0) {
    const height = pos.absolute.y - (contentNode.value?.getBoundingClientRect().top ?? 0)
    emit('update:height', Math.max(0, height))
  }
}, PointerButtonMask.Main)
</script>

<template>
  <Teleport to="body" :disabled="!fullscreen">
    <div
      ref="rootNode"
      class="VisualizationContainer"
      :class="{ fullscreen: fullscreen, 'below-node': belowNode, 'below-toolbar': belowToolbar }"
      :style="{
        '--color-visualization-bg': background,
      }"
    >
      <div class="resizer-right" v-on="resizeRight.events"></div>
      <div class="resizer-bottom" v-on="resizeBottom.events"></div>
      <div class="resizer-bottom-right" v-on="resizeBottomRight.events"></div>
      <div
        ref="contentNode"
        class="content scrollable"
        :class="{ overflow }"
        :style="{
          width: fullscreen ? undefined : `${width ?? nodeSize.x}px`,
          height: fullscreen ? undefined : `${height}px`,
        }"
        @wheel.passive="onWheel"
      >
        <slot></slot>
      </div>
      <div class="toolbars">
        <div v-if="!isCircularMenuVisible || fullscreen"></div>
        <div :class="{ toolbar: true, invisible: isCircularMenuVisible, hidden: fullscreen }">
          <div class="background"></div>
          <button class="image-button active" @click="emit('hide')">
            <img :src="EyeIcon" />
          </button>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <button class="image-button active" @click="emit('update:fullscreen', !fullscreen)">
            <img class="icon" :src="fullscreen ? ExitFullscreenIcon : FullscreenIcon" />
          </button>
          <div class="icon-container">
            <button
              class="image-button active"
              @click.stop="isSelectorVisible = !isSelectorVisible"
            >
              <img class="icon" :src="CompassIcon" />
            </button>
            <VisualizationSelector
              v-if="isSelectorVisible"
              :types="types"
              @hide="isSelectorVisible = false"
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
.VisualizationContainer {
  background: var(--color-visualization-bg);
  position: absolute;
  min-width: 100%;
  width: min-content;
  z-index: -1;
  border-radius: var(--radius-default);
}

.VisualizationContainer.below-node {
  padding-top: 36px;
}

.VisualizationContainer.below-toolbar {
  padding-top: 72px;
}

.VisualizationContainer.fullscreen {
  z-index: var(--z-fullscreen);
  position: fixed;
  padding-top: 0;
  border-radius: 0;
  left: 0;
  top: 0;
  width: 100vw;
  height: 100vh;
}

.VisualizationContainer.fullscreen.below-toolbar {
  padding-top: 38px;
}

.content {
  overflow: auto;
}

.content.overflow {
  overflow: visible;
}

.VisualizationContainer.fullscreen .content {
  height: 100%;
}

.toolbars {
  user-select: none;
  position: absolute;
  display: flex;
  gap: 4px;
  top: 36px;
}

.VisualizationContainer.fullscreen .toolbars {
  top: 4px;
}

.toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;

  > .background.background {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    border-radius: var(--radius-full);
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
}

.toolbar:not(:first-child):not(:has(> :nth-child(2))) {
  display: none;
}

.resizer-right {
  position: absolute;
  cursor: ew-resize;
  left: 100%;
  width: 12px;
  height: 100%;
}

.resizer-bottom {
  position: absolute;
  cursor: ns-resize;
  top: 100%;
  width: 100%;
  height: 12px;
}

.resizer-bottom-right {
  position: absolute;
  cursor: nwse-resize;
  left: calc(100% - 8px);
  top: calc(100% - 8px);
  width: 16px;
  height: 16px;
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

<style>
.VisualizationContainer > .toolbars > .toolbar > * {
  position: relative;
}

.image-button {
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
}

.image-button.active {
  cursor: pointer;
  opacity: unset;
}

.image-button > * {
  vertical-align: top;
}
</style>
