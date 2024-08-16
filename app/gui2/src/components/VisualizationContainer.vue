<script setup lang="ts">
import ResizeHandles from '@/components/ResizeHandles.vue'
import SvgButton from '@/components/SvgButton.vue'
import VisualizationSelector from '@/components/VisualizationSelector.vue'
import { isTriggeredByKeyboard } from '@/composables/events'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Rect, type BoundsSet } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { isQualifiedName, qnLastSegment } from '@/util/qualifiedName'
import { computed, ref, watch, watchEffect } from 'vue'

const props = defineProps<{
  /** If true, the visualization should be `overflow: visible` instead of `overflow: auto`. */
  overflow?: boolean
  /** If true, the visualization should display below the node background. */
  belowNode?: boolean
  /** If true, the visualization should display below the toolbar buttons. */
  belowToolbar?: boolean
  toolbarOverflow?: boolean
}>()

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

function hideSelector() {
  requestAnimationFrame(() => (isSelectorVisible.value = false))
}

const contentSize = computed(() => new Vec2(config.width, config.height))

// Because ResizeHandles are applying the screen mouse movements, the bouds must be in `screen`
// space.
const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, contentSize.value.scale(config.scale))
  },
  set(value) {
    if (resizing.left || resizing.right) config.width = value.width / config.scale
    if (resizing.bottom) config.height = value.height / config.scale
  },
})

let resizing: BoundsSet = {}

watch(contentSize, (newVal, oldVal) => {
  if (!resizing.left) return
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

const contentStyle = computed(() => {
  return {
    width: config.fullscreen ? undefined : `${config.width}px`,
    height: config.fullscreen ? undefined : `${config.height}px`,
  }
})

const overFlowStyle = computed(() => {
  return {
    overflow: props.toolbarOverflow ? 'visible' : 'hidden',
  }
})
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
        '--node-size-x': `${config.nodeSize.x}px`,
        '--node-size-y': `${config.nodeSize.y}px`,
        ...(config.isPreview ? { pointerEvents: 'none' } : {}),
      }"
    >
      <div
        ref="contentNode"
        class="content scrollable"
        :class="{ overflow: props.overflow }"
        :style="contentStyle"
        @wheel.passive="onWheel"
      >
        <slot></slot>
      </div>
      <ResizeHandles
        v-if="!config.isPreview"
        v-model="clientBounds"
        left
        right
        bottom
        @update:resizing="resizing = $event"
      />
      <div class="toolbars">
        <div
          v-if="!config.isPreview"
          :class="{
            toolbar: true,
            invisible: config.isCircularMenuVisible,
            hidden: config.fullscreen,
          }"
        >
          <SvgButton name="eye" alt="Hide visualization" @click.stop="config.hide()" />
        </div>
        <div v-if="!config.isPreview" class="toolbar">
          <SvgButton
            :name="config.fullscreen ? 'exit_fullscreen' : 'fullscreen'"
            :title="config.fullscreen ? 'Exit Fullscreen' : 'Fullscreen'"
            @click.stop.prevent="(config.fullscreen = !config.fullscreen), blur($event)"
          />
          <div class="icon-container">
            <SvgButton
              :name="config.icon ?? 'columns_increasing'"
              title="Visualization Selector"
              @click.stop.prevent="
                (!isSelectorVisible || isTriggeredByKeyboard($event)) &&
                  (isSelectorVisible = !isSelectorVisible)
              "
            />
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
        <div
          v-if="$slots.toolbar && !config.isPreview"
          id="visualization-defined-toolbar"
          class="visualization-defined-toolbars"
          :style="overFlowStyle"
        >
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
  --permanent-toolbar-width: 240px;
  --toolbar-reserved-height: 36px;
  --resize-handle-inside: var(--visualization-resize-handle-inside);
  --resize-handle-outside: var(--visualization-resize-handle-outside);
  --resize-handle-radius: var(--radius-default);
  color: var(--color-text);
  background: var(--color-visualization-bg);
  position: absolute;
  min-width: 100%;
  width: min-content;
  border-radius: var(--radius-default);
  cursor: default;
}

.VisualizationContainer {
  padding-top: calc(var(--node-size-y) - var(--radius-default));
}

.VisualizationContainer.below-node {
  padding-top: var(--node-size-y);
}

.VisualizationContainer.below-toolbar {
  padding-top: calc(var(--node-size-y) + var(--toolbar-reserved-height));
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
  contain: strict;
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
  top: calc(var(--node-size-y) + 4px);
}

.after-toolbars {
  display: flex;
  flex-direction: row;
  justify-content: flex-end;
  margin-left: auto;
  margin-right: 8px;
  overflow: hidden;
  width: calc(var(--node-size-x) - 200px);
}

.node-type {
  font-weight: bold;
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
  z-index: 20;

  &:before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
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
</style>
