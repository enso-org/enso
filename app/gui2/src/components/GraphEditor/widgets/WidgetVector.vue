<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { Tree, type Token } from '@/generated/ast'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { computed } from 'vue'

type Item = AstExtended<Tree.Tree | Token.Token, boolean>

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

function encodeAstPayload(ast: Item): string {
  return ast.repr()
}

function decodeAstPayload(id: string): Item {
  return AstExtended.parse(id)
}

const navigator = injectGraphNavigator(true)
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
    :getKey="(item: Item) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(item: Item) => item.repr()"
    :toDragPayload="encodeAstPayload"
    :fromDragPayload="decodeAstPayload"
    :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }">
      <NodeWidget :input="item" />
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
