<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import VectorWidget from '@/components/widgets/VectorWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const value = computed({
  get() {
    return props.input.children().filter((child) => child.isTree())
  },
  set(value) {
    const id = props.input.astId
    if (!id) return
    graph.replaceNodeSubexpression(
      id,
      undefined!,
      `[${value.map((item) => item.repr()).join(', ')}]`,
    )
  },
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AstExtended.isTree([Tree.Type.Array]), {
  priority: 1000,
  score: () => Score.Perfect,
})
</script>

<template>
  <VectorWidget
    v-slot="{ item }"
    v-model="value"
    :default="() => AstExtended.parse('_')"
    :getId="(item) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toDragPayload="(item) => ({ id: item.astId, code: item.repr() })"
    class="WidgetVector"
    contenteditable="false"
  >
    <NodeWidget :input="item" />
  </VectorWidget>
</template>

<style scoped>
:deep(.item.drag-preview) {
  background-color: var(--node-group-color);
}
</style>
