<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast, RawAst } from '@/util/ast'
import { SoCalledExpression } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const itemConfig = computed(() =>
  props.input.widgetConfig?.kind === 'Vector_Editor'
    ? props.input.widgetConfig.item_editor
    : undefined,
)

const defaultItem = computed(() => {
  const config = props.input.widgetConfig
  if (config?.kind === 'Vector_Editor') {
    return Ast.parse(config.item_default)
  } else {
    return Ast.Wildcard.new()
  }
})

const graph = useGraphStore()
const value = computed({
  get() {
    if (props.input.ast == null) return []
    return Array.from(props.input.ast.children()).filter(
      (child): child is Ast.Ast => child instanceof Ast.Ast,
    )
  },
  set(value) {
    // TODO[ao]: Handle placeholders instead of returning.
    if (!props.input.ast) return
    const newCode = `[${value.map((item) => item.code()).join(', ')}]`
    graph.setExpressionContent(props.input.ast.astId, newCode)
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(SoCalledExpression, {
  priority: 1000,
  score: (props) => {
    if (props.input.widgetConfig?.kind === 'Vector_Editor') return Score.Perfect
    else if (props.input.arg?.info.type === 'Standard.Base.Data.Vector') return Score.Good
    else
      return props.input.ast?.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="() => defaultItem"
    :getKey="(ast: Ast.Ast) => ast.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(ast: Ast.Ast) => ast.code()"
    :toDragPayload="(ast: Ast.Ast) => ast.serialize()"
    :fromDragPayload="Ast.deserialize"
    :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }">
      <NodeWidget :input="new SoCalledExpression(item, itemConfig)" />
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
