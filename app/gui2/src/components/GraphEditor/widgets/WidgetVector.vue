<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { Tree } from '@/generated/ast'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { Vec2 } from '@/util/vec2'
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

const navigator = injectGraphNavigator(true)

function toCSSTranslation(x: number, y: number) {
  if (!navigator) return ''
  const pos = navigator.clientToScenePos(new Vec2(x, y))
  return `translate(${pos.x}px, ${pos.y}px)`
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AstExtended.isTree([Tree.Type.Array]), {
  priority: 1000,
  score: () => Score.Perfect,
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="() => AstExtended.parse('_')"
    :getKey="(item) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(item) => item.repr()"
    :toDragPayload="(item) => JSON.stringify({ id: item.astId, code: item.repr() })"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }"><NodeWidget :input="item" /></template>
    <template #dragPreview="{ item, x, y }">
      <div class="drag-preview" :style="{ transform: toCSSTranslation(x, y) }">
        <NodeWidget :input="item" />
      </div>
    </template>
  </ListWidget>
</template>

<style scoped>
.drag-preview {
  position: fixed;
  background-color: var(--node-color-primary);
  border-radius: var(--node-border-radius);
}
</style>
