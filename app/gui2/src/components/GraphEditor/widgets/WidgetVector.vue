<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast, RawAst } from '@/util/ast'
import * as random from 'lib0/random'
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
    const newCode = `[${value.map((item) => item.repr()).join(', ')}]`
    graph.replaceExpressionContent(props.input.astId, newCode)
  },
})

function encodeAstPayload(ast: Ast.Ast): string {
  // TODO: serialize AST preserving metadata
  return ast.repr()
}

function decodeAstPayload(payload: string): Ast.Ast {
  // TODO: deserialize AST with preserved metadata
  return Ast.parse(payload)
}

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(Ast.Generic, {
  priority: 1000,
  score: (props) =>
    props.input.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch,
})

function defaultWildcard(): Ast.Wildcard {
  return new Ast.Wildcard(undefined, {
    node: new Ast.Token('_', random.uuidv4() as Ast.TokenId, RawAst.Token.Type.Wildcard),
  })
}
</script>

<template>
  <ListWidget
    v-model="value"
    :default="defaultWildcard"
    :getKey="(item: Ast.Ast) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(item) => item.repr()"
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
