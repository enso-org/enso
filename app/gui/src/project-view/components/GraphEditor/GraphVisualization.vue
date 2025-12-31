<script setup lang="ts">
import { visualizationBindings } from '@/bindings'
import type { RawDataSource } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import { useVisualizationData } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import VisualizationToolbar from '@/components/GraphEditor/GraphVisualization/VisualizationToolbar.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import ResizeHandles from '@/components/ResizeHandles.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { focusIsIn, useEvent, useResizeObserver } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import { injectResizableWidgetRegistry } from '@/providers/resizableWidgetRegistry'
import type { VisualizationDataSource } from '@/stores/visualization'
import type { Opt } from '@/util/data/opt'
import { Rect, type BoundsSet } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import type { ProjectPath } from '@/util/projectPath'
import { proxyRefs } from '@/util/reactivity'
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
  show: boolean
  currentType?: Opt<VisualizationIdentifier>
  isFullscreenAllowed: boolean
  isResizable: boolean
  isPreview?: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  width: Opt<number>
  height: Opt<number>
  scale: number
  isFocused: boolean
  /** @deprecated use typeinfo instead */
  typename?: ProjectPath | undefined
  typeinfo?: TypeInfo | undefined
  dataSource: VisualizationDataSource | RawDataSource | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  'update:id': [id: VisualizationIdentifier]
  'update:enabled': [visible: boolean]
  'update:width': [width: number]
  'update:height': [height: number]
  'update:nodePosition': [pos: Vec2]
  'update:hovered': [hovered: boolean]
  createNodes: [options: NodeCreationOptions[]]
}>()

// ===================================
// === Visualization-Specific Data ===
// ===================================

const {
  effectiveVisualization,
  effectiveVisualizationData,
  updatePreprocessor,
  allVisualizations,
  currentVisualization,
  setToolbarDefinition,
  visualizationDefinedToolbar,
  toolbarOverlay,
  executeExpression,
} = useVisualizationData({
  selectedVis: toRef(props, 'currentType'),
  dataSource: toRef(props, 'dataSource'),
  typename: toRef(props, 'typename'),
  typeinfo: toRef(props, 'typeinfo'),
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

const isFullscreen = ref(false)

const actionHandlers = registerHandlers({
  'visualization.exitFullscreen': {
    action: () => (isFullscreen.value = false),
  },
  'component.toggleVisualization': {
    available: () => !isFullscreen.value,
    action: () => emit('update:enabled', false),
  },
  'visualization.nextType': {
    action: () => {
      const currentIndex = allVisualizations.value.findIndex((type) =>
        visIdentifierEquals(type, currentVisualization.value),
      )
      const nextIndex = (currentIndex + 1) % allVisualizations.value.length
      emit('update:id', allVisualizations.value[nextIndex]!)
    },
  },
})

const keydownHandler = visualizationBindings.handler({
  'visualization.nextType': () => {
    if (props.isFocused || focusIsIn(panelElement.value)) {
      actionHandlers['visualization.nextType'].action()
    } else {
      return false
    }
  },
  'panel.fullscreen': () => {
    if (props.isFocused || focusIsIn(panelElement.value)) {
      isFullscreen.value = !isFullscreen.value
    } else {
      return false
    }
  },
  'visualization.exitFullscreen': () => {
    if (isFullscreen.value) {
      actionHandlers['visualization.exitFullscreen'].action()
    } else {
      return false
    }
  },
})

// TODO[ao]: we use `globalEventRegistryPre` to make sure it takes precedence before GraphEditor handlers
//  (deselectAllNodes in particular). But this is quick workaround, the proper solution
//  should be soon delivered as part of https://github.com/enso-org/enso/issues/13695
const { globalEventRegistryPre } = useGlobalEventRegistry()
useEvent(globalEventRegistryPre, 'keydown', keydownHandler)

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

// Use proxy object instead of computed to keep granular reactive updates across the `params` prop fields.
const visParams: VisualizationHostParams = proxyRefs({
  visualization: effectiveVisualization,
  data: effectiveVisualizationData,
  size: contentElementSize,
  nodeType: toRef(props, 'typename'),
  executeExpression,
})

const resizableWidgets = injectResizableWidgetRegistry(true)
</script>

<script lang="ts">
import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import VisualizationHost, {
  type VisualizationHostParams,
} from '@/components/visualizations/VisualizationHost.vue'
import { useGlobalEventRegistry } from '@/providers/globalEventRegistry'
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
  <div
    v-if="props.show"
    class="GraphVisualization"
    :style="style"
    :class="{ isFocused }"
    @pointerenter="emit('update:hovered', true)"
    @pointerleave="emit('update:hovered', false)"
  >
    <WithFullscreenMode
      v-model="isFullscreen"
      :enabled="isFullscreenAllowed"
      @update:animating="fullscreenAnimating = $event"
    >
      <div
        ref="panelElement"
        class="VisualizationPanel"
        :class="{
          nonInteractive: isPreview,
        }"
        tabindex="-1"
      >
        <VisualizationToolbar
          :currentVis="currentVisualization"
          :showControls="!isPreview"
          :isFocused="isFocused"
          :allVisualizations="allVisualizations"
          :visualizationDefinedToolbar="visualizationDefinedToolbar"
          :typename="typename"
          :typeinfo="typeinfo"
          :class="{ overlay: toolbarOverlay }"
          @update:currentVis="emit('update:id', $event)"
        />
        <div ref="contentElement" class="VisualizationHostContainer content scrollable">
          <component
            :is="ensoVisualizationHost"
            :params="visParams"
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
      v-on="resizableWidgets?.visResizeHandleEventHandlers"
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
  opacity: 0.9;
  overflow: hidden;
  transition: opacity 0.2s;
  &.isFocused {
    opacity: 1;
  }
}

.VisualizationPanel {
  --permanent-toolbar-width: 240px;
  color: var(--color-text);
  cursor: default;
  position: relative;
  display: flex;
  flex-direction: column;
  height: 100%;
  background: var(--color-visualization-bg);
}

.content {
  overflow: auto;
  contain: strict;
  isolation: isolate;
  border-radius: 0 0 var(--radius-default) var(--radius-default);
  height: 100%;
  overscroll-behavior: contain;
}

.nonInteractive {
  pointer-events: none;
}

.overlay {
  position: absolute;
  z-index: 1;
}
</style>
