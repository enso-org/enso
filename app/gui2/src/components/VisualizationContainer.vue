<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import VisualizationSelector from '@/components/VisualizationSelector.vue'

import { PointerButtonMask, usePointer } from '@/util/events'

import { ref } from 'vue'
import { useVisualizationConfig } from '../providers/visualizationConfig'

const props = defineProps<{
  /** If true, the visualization should be `overflow: visible` instead of `overflow: hidden`. */
  overflow?: boolean
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
}>()

const config = useVisualizationConfig()

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
  config.value.width = Math.max(config.value.nodeSize.x, width)
}, PointerButtonMask.Main)

const resizeBottom = usePointer((pos, _, type) => {
  if (type !== 'move' || pos.delta.y === 0) {
    return
  }
  const height = pos.absolute.y - (contentNode.value?.getBoundingClientRect().top ?? 0)
  config.value.height = Math.max(0, height)
}, PointerButtonMask.Main)

const resizeBottomRight = usePointer((pos, _, type) => {
  if (type !== 'move') {
    return
  }
  if (pos.delta.x !== 0) {
    const width = pos.absolute.x - (contentNode.value?.getBoundingClientRect().left ?? 0)
    config.value.width = Math.max(config.value.nodeSize.x, width)
  }
  if (pos.delta.y !== 0) {
    const height = pos.absolute.y - (contentNode.value?.getBoundingClientRect().top ?? 0)
    config.value.height = Math.max(0, height)
  }
}, PointerButtonMask.Main)
</script>

<template>
  <Teleport to="body" :disabled="!config.fullscreen">
    <div
      ref="rootNode"
      class="VisualizationContainer"
      :class="{
        fullscreen: config.fullscreen,
        'circular-menu-visible': config.isCircularMenuVisible,
        'below-node': props.belowNode,
        'below-toolbar': props.belowToolbar,
      }"
      :style="{
        '--color-visualization-bg': config.background,
      }"
    >
      <div class="resizer-right" v-on="resizeRight.stop.events"></div>
      <div class="resizer-bottom" v-on="resizeBottom.stop.events"></div>
      <div class="resizer-bottom-right" v-on="resizeBottomRight.stop.events"></div>
      <div
        ref="contentNode"
        class="content scrollable"
        :class="{ overflow: props.overflow }"
        :style="{
          width: config.fullscreen
            ? undefined
            : `${Math.max(config.width ?? 0, config.nodeSize.x)}px`,
          height: config.fullscreen ? undefined : `${config.height}px`,
        }"
        @wheel.passive="onWheel"
      >
        <slot></slot>
      </div>
      <div class="toolbars">
        <div
          :class="{
            toolbar: true,
            invisible: config.isCircularMenuVisible,
            hidden: config.fullscreen,
          }"
        >
          <div class="background"></div>
          <button class="image-button active" @pointerdown.stop="config.hide()">
            <SvgIcon class="icon" name="eye" />
          </button>
        </div>
        <div class="toolbar">
          <div class="background"></div>
          <button
            class="image-button active"
            @pointerdown.stop="config.fullscreen = !config.fullscreen"
          >
            <SvgIcon class="icon" :name="config.fullscreen ? 'exit_fullscreen' : 'fullscreen'" />
          </button>
          <div class="icon-container">
            <button
              class="image-button active"
              @pointerdown.stop="isSelectorVisible = !isSelectorVisible"
            >
              <SvgIcon class="icon" name="compass" />
            </button>
            <VisualizationSelector
              v-if="isSelectorVisible"
              :types="config.types"
              @hide="isSelectorVisible = false"
              @update:type="
                (type) => {
                  isSelectorVisible = false
                  config.updateType(type)
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
  color: var(--color-text);
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

.VisualizationContainer.fullscreen.below-node {
  padding-top: 0;
}

.VisualizationContainer.fullscreen.below-toolbar {
  padding-top: 38px;
}

.toolbars {
  transition-duration: 100ms;
  transition-property: padding-left;
}

.VisualizationContainer.fullscreen .toolbars,
.VisualizationContainer:not(.circular-menu-visible) .toolbars {
  padding-left: 4px;
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

.VisualizationContainer.below-node > .resizer-right {
  height: calc(100% - 36px);
}

.VisualizationContainer.below-toolbar > .resizer-right {
  height: calc(100% - 72px);
}

.VisualizationContainer.fullscreen.below-node > .resizer-right {
  height: 100%;
}

.VisualizationContainer.fullscreen.below-toolbar > .resizer-right {
  height: calc(100% - 38px);
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
  cursor: none;
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
}

.image-button.active {
  opacity: unset;
}

.image-button > * {
  vertical-align: top;
}
</style>
