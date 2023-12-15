<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { ForcePort } from '@/providers/portInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast, RawAst } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const value = computed({
  get() {
    return Array.from(props.input.children()).filter(
      (child): child is Ast.Ast => child instanceof Ast.Ast,
    )
  },
  set(value) {
    const newCode = `[${value.map((item) => item.code()).join(', ')}]`
    graph.setExpressionContent(props.input.astId, newCode)
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
function forcePort(item: WidgetInput) {
  return item instanceof Ast.Ast ? new ForcePort(item) : item
}

export const widgetDefinition = defineWidget(Ast.Ast, {
  priority: 1000,
  score: (props) =>
    props.input.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch,
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="Ast.Wildcard.new"
    :getKey="(item: Ast.Ast) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(item: Ast.Ast) => item.code()"
    :toDragPayload="(ast: Ast.Ast) => ast.serialize()"
    :fromDragPayload="Ast.deserialize"
    :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }">
      <NodeWidget :input="forcePort(item)" />
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
