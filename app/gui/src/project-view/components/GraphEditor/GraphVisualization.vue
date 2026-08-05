<script setup lang="ts">
import { proxyRefs } from '$/utils/reactivity'
import { visualizationBindings } from '@/bindings'
import type { RawDataSource } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import { useVisualizationData } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import VisualizationToolbar from '@/components/GraphEditor/GraphVisualization/VisualizationToolbar.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { useResizeHandles } from '@/components/resizeHandles'
import ResizeHandles from '@/components/ResizeHandles.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { focusIsIn, useEvent, useResizeObserver } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import type { VisualizationDataSource } from '@/stores/visualization'
import type { Opt } from '@/util/data/opt'
import { Vec2 } from '@/util/data/vec2'
import { computed, nextTick, ref, toRef, watch, watchEffect } from 'vue'
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
  typeinfo?: TypeInfo | undefined
  dataSource: VisualizationDataSource | RawDataSource | undefined
}>()
const emit = defineEmits<{
  'update:effectiveSize': [size: Vec2]
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

function clampSize(x: Opt<number>, y: Opt<number>) {
  return new Vec2(
    Math.max(x ?? 0, MIN_WIDTH_PX),
    Math.max(y ?? DEFAULT_CONTENT_HEIGHT_PX, MIN_CONTENT_HEIGHT_PX),
  )
}

const vizSize = computed<Vec2>(() =>
  clampSize(Math.max(props.width ?? 0, props.nodeSize.x), props.height),
)

watchEffect(() => emit('update:effectiveSize', vizSize.value))

const requestedSize = ref<Vec2>()
watch(requestedSize, (size, oldSize) => {
  if (size && size.x !== oldSize?.x) emit('update:width', size.x)
  if (size && size.y !== oldSize?.y) emit('update:height', size.y)
})

const resizeHandles = useResizeHandles({
  size: vizSize,
  position: toRef(props, 'nodePosition'),
  scale: toRef(props, 'scale'),
})
resizeHandles.onResizeWidth((value) => emit('update:width', value))
resizeHandles.onResizeHeight((value) => emit('update:height', value))
resizeHandles.onMove((position) => emit('update:nodePosition', position))

const style = computed(() => {
  return {
    'padding-top': `${props.nodeSize.y}px`,
    width: `${vizSize.value.x}px`,
    height: `${vizSize.value.y + props.nodeSize.y}px`,
  }
})

const fullscreenAnimating = ref(false)

watch(
  () => isFullscreen,
  (f) => f && nextTick(() => panelElement.value?.focus()),
)

const nodeType = computed(() => props.typeinfo?.primaryType ?? undefined)
// Use proxy object instead of computed to keep granular reactive updates across the `params` prop fields.
const visParams: VisualizationHostParams = proxyRefs({
  visualization: effectiveVisualization,
  data: effectiveVisualizationData,
  size: contentElementSize,
  nodeType,
  executeExpression,
})
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
    <ResizeHandles v-if="!isPreview && isResizable" left right bottom v-on="resizeHandles.events" />
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
