<script setup lang="ts">
import SmallPlusButton from '@/components/SmallPlusButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import VisualizationSelector from '@/components/VisualizationSelector.vue'
import {
  PointerButtonMask,
  isTriggeredByKeyboard,
  usePointer,
  useResizeObserver,
} from '@/composables/events'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Vec2 } from '@/util/data/vec2'
import { isQualifiedName, qnLastSegment } from '@/util/qualifiedName'
import { computed, onMounted, ref, watch, watchEffect } from 'vue'

const props = defineProps<{
  /** If true, the visualization should be `overflow: visible` instead of `overflow: hidden`. */
  overflow?: boolean
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
}>()

/** The minimum width must be at least the total width of:
 * - both of toolbars that are always visible (32px + 60px), and
 * - the 4px flex gap between the toolbars. */
const MIN_WIDTH_PX = 200

const config = useVisualizationConfig()

watchEffect(() => (config.isBelowToolbar = props.belowToolbar))

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

function blur(event: Event) {
  const target = event.target
  if (
    !(target instanceof HTMLElement) &&
    !(target instanceof SVGElement) &&
    !(target instanceof MathMLElement)
  )
    return
  setTimeout(() => target.blur(), 0)
}

const contentNode = ref<HTMLElement>()

onMounted(() => (config.width = MIN_WIDTH_PX))

function hideSelector() {
  requestAnimationFrame(() => (isSelectorVisible.value = false))
}

const realSize = useResizeObserver(contentNode)

let initialWidth: number | undefined
let initialHeight: number | undefined
let leftDragging: boolean = false
function resizeHandler(resizeX: 'left' | 'right' | false, resizeY: boolean) {
  return usePointer((pos, _, type) => {
    switch (type) {
      case 'start':
        initialWidth = realSize.value.x
        initialHeight = realSize.value.y
        leftDragging = resizeX === 'left'
        break
      case 'move':
        if (resizeX === 'right' && pos.delta.x !== 0) {
          const width = (initialWidth ?? 0) + pos.relative.x / config.scale
          config.width = Math.max(0, width)
        }
        if (resizeX === 'left' && pos.delta.x !== 0) {
          const width = (initialWidth ?? 0) - pos.relative.x / config.scale
          config.width = Math.max(0, width)
          // Updating the node's position is done in the watch below.
        }
        if (resizeY && pos.delta.y !== 0) {
          const height = (initialHeight ?? 0) + pos.relative.y / config.scale
          config.height = Math.max(0, height)
        }
        break
      case 'stop':
        leftDragging = false
        break
    }
  })
}

const resizeRight = resizeHandler('right', false)
const resizeLeft = resizeHandler('left', false)
const resizeBottom = resizeHandler(false, true)
const resizeBottomRight = resizeHandler('right', true)
const resizeBottomLeft = resizeHandler('left', true)

// When dragging left resizer, we need to move node position accordingly. It may be done only by
// reading the real width change, as `config.width` does not consider node's minimum width.
watch(realSize, (newVal, oldVal) => {
  if (!leftDragging) return
  const delta = newVal.x - oldVal.x
  if (delta !== 0)
    config.nodePosition = new Vec2(config.nodePosition.x - delta, config.nodePosition.y)
})

const UNKNOWN_TYPE = 'Unknown'
const nodeShortType = computed(() =>
  config.nodeType != null && isQualifiedName(config.nodeType) ?
    qnLastSegment(config.nodeType)
  : UNKNOWN_TYPE,
)
</script>

<template>
  <Teleport to="body" :disabled="!config.fullscreen">
    <div
      class="VisualizationContainer"
      :class="{
        fullscreen: config.fullscreen,
        'circular-menu-visible': config.isCircularMenuVisible,
        'below-node': props.belowNode,
        'below-toolbar': props.belowToolbar,
      }"
      :style="{
        '--color-visualization-bg': config.background,
        '--node-height': `${config.nodeSize.y}px`,
      }"
    >
      <SmallPlusButton
        v-if="config.isCircularMenuVisible"
        class="below-viz"
        @createNodes="config.createNodes(...$event)"
      />
      <div
        ref="contentNode"
        class="content scrollable"
        :class="{ overflow: props.overflow }"
        :style="{
          width:
            config.fullscreen ? undefined : `${Math.max(config.width ?? 0, config.nodeSize.x)}px`,
          height:
            config.fullscreen ? undefined : `${Math.max(config.height ?? 0, config.nodeSize.y)}px`,
        }"
        @wheel.passive="onWheel"
      >
        <slot></slot>
      </div>
      <div class="resizer-left" v-on="resizeLeft.events" />
      <div class="resizer-right" v-on="resizeRight.events" />
      <div class="resizer-bottom" v-on="resizeBottom.events" />
      <div class="resizer-bottom-left" v-on="resizeBottomLeft.events" />
      <div class="resizer-bottom-right" v-on="resizeBottomRight.events" />
      <SmallPlusButton
        v-if="config.isCircularMenuVisible"
        class="below-viz"
        @createNodes="config.createNodes(...$event)"
      />
      <div class="toolbars">
        <div
          :class="{
            toolbar: true,
            invisible: config.isCircularMenuVisible,
            hidden: config.fullscreen,
          }"
        >
          <button class="image-button active" @click.stop="config.hide()">
            <SvgIcon class="icon" name="eye" alt="Hide visualization" />
          </button>
        </div>
        <div class="toolbar">
          <button
            class="image-button active"
            @click.stop.prevent="(config.fullscreen = !config.fullscreen), blur($event)"
          >
            <SvgIcon
              class="icon"
              :name="config.fullscreen ? 'exit_fullscreen' : 'fullscreen'"
              :alt="config.fullscreen ? 'Exit fullscreen' : 'Enter fullscreen'"
            />
          </button>
          <div class="icon-container">
            <button
              class="image-button active"
              @click.stop.prevent="
                (!isSelectorVisible || isTriggeredByKeyboard($event)) &&
                  (isSelectorVisible = !isSelectorVisible)
              "
            >
              <SvgIcon
                class="icon"
                :name="config.icon ?? 'columns_increasing'"
                :alt="
                  isSelectorVisible ? 'Hide visualization selector' : 'Show visualization selector'
                "
              />
            </button>
            <Suspense>
              <VisualizationSelector
                v-if="isSelectorVisible"
                :types="config.types"
                :modelValue="config.currentType"
                @hide="hideSelector"
                @update:modelValue="(isSelectorVisible = false), config.updateType($event)"
              />
            </Suspense>
          </div>
        </div>
        <div v-if="$slots.toolbar" class="visualization-defined-toolbars">
          <div class="toolbar"><slot name="toolbar"></slot></div>
        </div>
        <div
          class="after-toolbars node-type"
          :title="config.nodeType ?? UNKNOWN_TYPE"
          v-text="nodeShortType"
        />
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.VisualizationContainer {
  --node-height: 32px;
  --permanent-toolbar-width: 200px;
  color: var(--color-text);
  background: var(--color-visualization-bg);
  position: absolute;
  min-width: 100%;
  width: min-content;
  border-radius: var(--radius-default);
  cursor: default;
}

.VisualizationContainer.below-node {
  padding-top: --node-height;
}

.VisualizationContainer.below-toolbar {
  padding-top: calc(var(--node-height) + 40px);
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
  padding-top: 40px;
}

.VisualizationContainer.fullscreen.below-toolbar {
  padding-top: 40px;
}

.toolbars {
  transition-duration: 100ms;
  transition-property: padding-left;
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
  width: 100%;
  user-select: none;
  position: absolute;
  display: flex;
  gap: 4px;
  top: calc(var(--node-height) + 4px);
}

.after-toolbars {
  margin-left: auto;
  margin-right: 8px;
}

.node-type {
  font-weight: bold;
}

.VisualizationContainer.fullscreen .toolbars {
  top: 4px;
}

.below-viz {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}

.toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
  z-index: 20;

  &:before {
    content: '';
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

.toolbar:not(:first-child):not(:has(> *)) {
  display: none;
}

.visualization-defined-toolbars {
  max-width: calc(100% - var(--permanent-toolbar-width));
  /* FIXME [sb]: This will cut off floating panels - consider investigating whether there's a better
   * way to clip only the toolbar div itself. */
  overflow-x: hidden;
}

.resizer-right {
  position: absolute;
  cursor: ew-resize;
  left: calc(100% - var(--visualization-resize-handle-inside));
  width: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
  top: 0px;
  height: 100%;
}

.resizer-left {
  position: absolute;
  cursor: ew-resize;
  left: calc(0px - var(--visualization-resize-handle-outside));
  width: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
  top: 0px;
  height: 100%;
}

.resizer-bottom {
  position: absolute;
  cursor: ns-resize;
  top: calc(100% - var(--visualization-resize-handle-inside));
  width: 100%;
  height: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
}

.resizer-bottom-right {
  position: absolute;
  cursor: nwse-resize;
  left: calc(100% - var(--visualization-resize-handle-inside));
  top: calc(100% - var(--visualization-resize-handle-inside));
  width: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
  height: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
}

.resizer-bottom-left {
  position: absolute;
  cursor: nesw-resize;
  left: calc(0px - var(--visualization-resize-handle-inside));
  top: calc(100% - var(--visualization-resize-handle-inside));
  width: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
  height: calc(
    var(--visualization-resize-handle-inside) + var(--visualization-resize-handle-outside)
  );
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

.VisualizationContainer :deep(> .toolbars > .toolbar > *) {
  position: relative;
}

:deep(.image-button) {
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
}

:deep(.image-button.active) {
  opacity: unset;
}

:deep(.image-button > *) {
  vertical-align: top;
}
</style>
