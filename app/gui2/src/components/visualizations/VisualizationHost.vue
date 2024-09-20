<script setup lang="ts">
import { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { ToolbarItem } from '@/components/visualizations/toolbar'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { Vec2 } from '@/util/data/vec2'
import { ToValue } from '@/util/reactivity'

const { visualization, data, size, nodeType } = defineProps<{
  visualization?: string | object
  data?: any
  size: Vec2
  nodeType?: string | undefined
  overflow?: boolean
  toolbarOverflow?: boolean
}>()

const emit = defineEmits<{
  updatePreprocessor: [
    visualizationModule: string,
    expression: string,
    ...positionalArgumentsExpressions: string[],
  ]
  updateToolbar: [items: ToValue<Readonly<ToolbarItem[]>>]
  updateToolbarOverlay: [enable: boolean]
  createNodes: [nodes: NodeCreationOptions[]]
}>()

// =========================
// === Visualization API ===
// =========================

provideVisualizationConfig({
  get size() {
    return size
  },
  get nodeType() {
    return nodeType
  },
  setPreprocessor: (
    visualizationModule: string,
    expression: string,
    ...positionalArgumentsExpressions: string[]
  ) =>
    emit('updatePreprocessor', visualizationModule, expression, ...positionalArgumentsExpressions),
  setToolbar: (items) => emit('updateToolbar', items),
  setToolbarOverlay: (overlay) => emit('updateToolbarOverlay', overlay),
  createNodes: (...nodes) => emit('createNodes', nodes),
})
</script>

<template>
  <Suspense>
    <template #fallback><LoadingVisualization /></template>
    <component :is="visualization" v-if="visualization && data" :data="data" />
    <LoadingVisualization v-else />
  </Suspense>
</template>

<style>
:host {
  display: block;
  width: 100%;
  height: 100%;
}

:host([hidden]) {
  display: none;
}

/* Base style for visualizations. */
:host {
  --color-text: rgb(118 118 118);
  --font-sans: 'M PLUS 1', /* System sans-serif font stack */ system-ui, -apple-system,
    BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans',
    'Droid Sans', 'Helvetica Neue', Arial, sans-serif;
  --font-mono: 'DejaVu Sans Mono', /* System monospace font stack */ ui-monospace, Menlo, Monaco,
    'Cascadia Mono', 'Segoe UI Mono', 'Roboto Mono', 'Oxygen Mono', 'Ubuntu Monospace',
    'Source Code Pro', 'Fira Mono', 'Droid Sans Mono', 'Courier New', monospace;
  color: var(--color-text);
  font-family: var(--font-sans);
  font-weight: 500;
  font-size: 11.5px;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  pointer-events: all;
  cursor: default;
}

*,
*::before,
*::after {
  box-sizing: border-box;
  margin: 0;
}

/* Scrollbar style definitions for textual visualizations which need support for scrolling.
 *
 * The 11px width/height (depending on scrollbar orientation)
 * is set so that it resembles macOS default scrollbar.
 */

.scrollable {
  scrollbar-color: rgba(190 190 190 / 50%) transparent;
  &::-webkit-scrollbar {
    -webkit-appearance: none;
  }
  &::-webkit-scrollbar-track {
    -webkit-box-shadow: none;
  }
  &::-webkit-scrollbar:vertical {
    width: 11px;
  }
  &::-webkit-scrollbar:horizontal {
    height: 11px;
  }
  &::-webkit-scrollbar-thumb {
    border-radius: 8px;
    border: 1px solid rgba(220, 220, 220, 0.5);
    background-color: rgba(190, 190, 190, 0.5);
  }
  &::-webkit-scrollbar-corner {
    background: rgba(0, 0, 0, 0);
  }
  &::-webkit-scrollbar-button {
    height: 8px;
    width: 8px;
  }
}

.draggable {
  cursor: grab;
}

.clickable {
  cursor: pointer;
}
</style>
