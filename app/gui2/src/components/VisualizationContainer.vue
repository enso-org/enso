<script setup lang="ts">
/** @file The layout of a visualization within a node. */

import ResizeHandles from '@/components/ResizeHandles.vue'
import VisualizationPanel from '@/components/VisualizationPanel.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Rect, type BoundsSet } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, watch, watchEffect } from 'vue'

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

const contentSize = computed(() => new Vec2(config.width, config.height))

// Because ResizeHandles are applying the screen mouse movements, the bounds must be in `screen`
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

const style = computed(() => {
  return {
    '--color-visualization-bg': config.background,
    '--node-size-x': `${config.nodeSize.x}px`,
    '--node-size-y': `${config.nodeSize.y}px`,
    width: `${config.width}px`,
    '--content-height': `${config.height}px`,
  }
})
</script>

<template>
  <div
    class="VisualizationContainer"
    :class="{
      'below-node': props.belowNode,
      'below-toolbar': props.belowToolbar,
    }"
    :style="style"
  >
    <VisualizationPanel :overflow="overflow ?? false" :toolbarOverflow="toolbarOverflow ?? false">
      <template v-if="$slots.toolbar" #toolbar><slot name="toolbar" /></template>
      <template #default><slot /></template>
    </VisualizationPanel>
    <ResizeHandles
      v-if="!config.isPreview && config.isResizable"
      v-model="clientBounds"
      left
      right
      bottom
      @update:resizing="resizing = $event"
    />
  </div>
</template>

<style scoped>
.VisualizationContainer {
  --resize-handle-inside: var(--visualization-resize-handle-inside);
  --resize-handle-outside: var(--visualization-resize-handle-outside);
  --resize-handle-radius: var(--radius-default);
  --toolbar-reserved-height: 36px;
  position: absolute;
  border-radius: var(--radius-default);
  background: var(--color-visualization-bg);
}

.VisualizationContainer {
  padding-top: var(--node-size-y);
  height: calc(var(--content-height) + var(--node-size-y));
}

.VisualizationContainer.below-node {
  padding-top: var(--node-size-y);
  height: calc(var(--content-height) + var(--node-size-y));
}

.VisualizationContainer.below-toolbar {
  padding-top: var(--node-size-y);
  height: calc(var(--content-height) + var(--node-size-y) + var(--toolbar-reserved-height));
}
</style>
