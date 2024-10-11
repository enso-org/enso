<script setup lang="ts">
import { visualizationBindings } from '@/bindings'
import {
  RawDataSource,
  useVisualizationData,
} from '@/components/GraphEditor/GraphVisualization/visualizationData'
import VisualizationToolbar from '@/components/GraphEditor/GraphVisualization/VisualizationToolbar.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import ResizeHandles from '@/components/ResizeHandles.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { focusIsIn, useEvent, useResizeObserver } from '@/composables/events'
import { VisualizationDataSource } from '@/stores/visualization'
import type { Opt } from '@/util/data/opt'
import { type BoundsSet, Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, nextTick, onUnmounted, ref, toRef, watch, watchEffect } from 'vue'
import { visIdentifierEquals, type VisualizationIdentifier } from 'ydoc-shared/yjsModel'

/**
 * The minimum width must be at least the total width of:
 * - both of toolbars that are always visible (32px + 60px), and
 * - the 4px flex gap between the toolbars.
 */
const MIN_WIDTH_PX = 200
const MIN_CONTENT_HEIGHT_PX = 32
const DEFAULT_CONTENT_HEIGHT_PX = 150

const props = defineProps<{
  currentType?: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  isFullscreenAllowed: boolean
  isResizable: boolean
  isPreview?: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  width: Opt<number>
  height: Opt<number>
  scale: number
  isFocused: boolean
  typename?: string | undefined
  dataSource: VisualizationDataSource | RawDataSource | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  'update:id': [id: VisualizationIdentifier]
  'update:enabled': [visible: boolean]
  'update:width': [width: number]
  'update:height': [height: number]
  'update:nodePosition': [pos: Vec2]
  createNodes: [options: NodeCreationOptions[]]
}>()

// ===================================
// === Visualization-Specific Data ===
// ===================================

const {
  effectiveVisualization,
  effectiveVisualizationData,
  updatePreprocessor,
  allTypes,
  currentType,
  setToolbarDefinition,
  visualizationDefinedToolbar,
  toolbarOverlay,
} = useVisualizationData({
  selectedVis: toRef(props, 'currentType'),
  dataSource: toRef(props, 'dataSource'),
  typename: toRef(props, 'typename'),
})

// ===========
// === DOM ===
// ===========

/** Includes content and toolbars. */
const panelElement = ref<HTMLElement>()

/** Contains only the visualization itself. */
const contentElement = ref<HTMLElement>()
const contentElementSize = useResizeObserver(contentElement)

// === Events ===

const keydownHandler = visualizationBindings.handler({
  nextType: () => {
    if (props.isFocused || focusIsIn(panelElement.value)) {
      const currentIndex = allTypes.value.findIndex((type) =>
        visIdentifierEquals(type, currentType.value),
      )
      const nextIndex = (currentIndex + 1) % allTypes.value.length
      emit('update:id', allTypes.value[nextIndex]!)
    } else {
      return false
    }
  },
  toggleFullscreen: () => {
    if (props.isFocused || focusIsIn(panelElement.value)) {
      isFullscreen.value = !isFullscreen.value
    } else {
      return false
    }
  },
  exitFullscreen: () => {
    if (isFullscreen.value) {
      isFullscreen.value = false
    } else {
      return false
    }
  },
})

useEvent(window, 'keydown', keydownHandler)

function onWheel(event: WheelEvent) {
  if (
    event.currentTarget instanceof Element &&
    (isFullscreen.value ||
      event.currentTarget.scrollWidth > event.currentTarget.clientWidth ||
      event.currentTarget.scrollHeight > event.currentTarget.clientHeight)
  ) {
    event.stopPropagation()
  }
}

// =============================
// === Sizing and Fullscreen ===
// =============================

const rect = computed(
  () =>
    new Rect(
      props.nodePosition,
      new Vec2(
        Math.max(props.width ?? MIN_WIDTH_PX, props.nodeSize.x),
        Math.max(props.height ?? DEFAULT_CONTENT_HEIGHT_PX, MIN_CONTENT_HEIGHT_PX) +
          props.nodeSize.y,
      ),
    ),
)

watchEffect(() => emit('update:rect', rect.value))
onUnmounted(() => emit('update:rect', undefined))

const isFullscreen = ref(false)

const containerContentSize = computed<Vec2>(
  () => new Vec2(rect.value.width, rect.value.height - props.nodeSize.y),
)

// Because ResizeHandles are applying the screen mouse movements, the bounds must be in `screen`
// space.
const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, containerContentSize.value.scale(props.scale))
  },
  set(value) {
    if (resizing.left || resizing.right) emit('update:width', value.width / props.scale)
    if (resizing.bottom) emit('update:height', value.height / props.scale)
  },
})

// It's not const, because it's assigned in an event handler in template.
// eslint-disable-next-line prefer-const
let resizing: BoundsSet = {}

watch(containerContentSize, (newVal, oldVal) => {
  if (!resizing.left) return
  const delta = newVal.x - oldVal.x
  if (delta !== 0)
    emit('update:nodePosition', new Vec2(props.nodePosition.x - delta, props.nodePosition.y))
})

const style = computed(() => {
  return {
    'padding-top': `${props.nodeSize.y}px`,
    width: `${rect.value.width}px`,
    height: `${rect.value.height}px`,
  }
})

const fullscreenAnimating = ref(false)

watch(
  () => isFullscreen,
  (f) => f && nextTick(() => panelElement.value?.focus()),
)
</script>

<script lang="ts">
import VisualizationHost from '@/components/visualizations/VisualizationHost.vue'
import { defineCustomElement } from 'vue'

// ==========================
// === Visualization Host ===
// ==========================

let definitionNumber = 0
if (import.meta.hot) {
  import.meta.hot.data.graphVizDefinitionNumber =
    (import.meta.hot.data.graphVizDefinitionNumber ?? 0) + 1
  definitionNumber = import.meta.hot.data.graphVizDefinitionNumber
}
const ensoVisualizationHost = `enso-visualization-host-${definitionNumber}`
customElements.define(ensoVisualizationHost, defineCustomElement(VisualizationHost))
</script>

<template>
  <div class="GraphVisualization" :style="style">
    <WithFullscreenMode :fullscreen="isFullscreen" @update:animating="fullscreenAnimating = $event">
      <div
        ref="panelElement"
        class="VisualizationPanel"
        :class="{
          fullscreen: isFullscreen || fullscreenAnimating,
          nonInteractive: isPreview,
        }"
        tabindex="-1"
      >
        <VisualizationToolbar
          v-model:isFullscreen="isFullscreen"
          :currentVis="currentType"
          :showControls="!isPreview"
          :hideVisualizationButton="
            isFullscreen ? 'hide'
            : isCircularMenuVisible ? 'invisible'
            : 'show'
          "
          :isFullscreenAllowed="isFullscreenAllowed"
          :allTypes="allTypes"
          :visualizationDefinedToolbar="visualizationDefinedToolbar"
          :typename="typename"
          :class="{ overlay: toolbarOverlay }"
          @update:currentVis="emit('update:id', $event)"
          @hide="emit('update:enabled', false)"
        />
        <div
          ref="contentElement"
          class="VisualizationHostContainer content scrollable"
          @wheel.passive="onWheel"
        >
          <component
            :is="ensoVisualizationHost"
            :visualization="effectiveVisualization"
            :data="effectiveVisualizationData"
            :size="contentElementSize"
            :nodeType="typename"
            @updatePreprocessor="
              updatePreprocessor($event.detail[0], $event.detail[1], ...$event.detail.slice(2))
            "
            @updateToolbar="setToolbarDefinition($event.detail[0])"
            @updateToolbarOverlay="toolbarOverlay = $event.detail[0]"
            @createNodes="emit('createNodes', $event.detail[0])"
          />
        </div>
      </div>
    </WithFullscreenMode>
    <ResizeHandles
      v-if="!isPreview && isResizable"
      v-model="clientBounds"
      left
      right
      bottom
      @update:resizing="resizing = $event"
    />
  </div>
</template>

<style scoped>
.GraphVisualization {
  --resize-handle-inside: var(--visualization-resize-handle-inside);
  --resize-handle-outside: var(--visualization-resize-handle-outside);
  --resize-handle-radius: var(--radius-default);
  position: absolute;
  border-radius: var(--radius-default);
  background: var(--color-visualization-bg);
  /** Prevent drawing on top of other UI elements (e.g. dropdown widgets). */
  isolation: isolate;
}

.VisualizationPanel {
  --permanent-toolbar-width: 240px;
  color: var(--color-text);
  cursor: default;
  position: relative;
  display: flex;
  flex-direction: column;
  height: 100%;
  &.fullscreen {
    background: var(--color-visualization-bg);
  }
}

.content {
  overflow: auto;
  contain: strict;
  height: 100%;
}

.nonInteractive {
  pointer-events: none;
}

.overlay {
  position: absolute;
}
</style>
