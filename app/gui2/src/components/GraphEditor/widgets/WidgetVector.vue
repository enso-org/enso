<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast, RawAst } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract.ts'
import { asNot } from '@/util/data/types.ts'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const itemConfig = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor'
    ? props.input.dynamicConfig.item_editor
    : undefined,
)

const defaultItem = computed(() => {
  if (props.input.dynamicConfig?.kind === 'Vector_Editor') {
    return Ast.parse(props.input.dynamicConfig.item_default)
  } else {
    return Ast.Wildcard.new()
  }
})

const value = computed({
  get() {
    if (!(props.input.value instanceof Ast.Ast)) return []
    return Array.from(props.input.value.children()).filter(
      (child): child is Ast.Ast => child instanceof Ast.Ast,
    )
  },
  set(value) {
    // TODO[ao]: here we re-create AST. It would be better to reuse existing AST nodes.
    const newCode = `[${value.map((item) => item.code()).join(', ')}]`
    const edit = graph.astModule.edit()
    props.onUpdate({
      edit,
      portUpdate: { value: newCode, origin: asNot<TokenId>(props.input.portId) },
    })
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 1001,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Vector_Editor') return Score.Perfect
    else if (props.input.expectedType?.startsWith('Standard.Base.Data.Vector.Vector'))
      return Score.Good
    else if (props.input.value instanceof Ast.Ast)
      return props.input.value.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch
    else return Score.Mismatch
  },
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="() => defaultItem"
    :getKey="(ast: Ast.Ast) => ast.id"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(ast: Ast.Ast) => ast.code()"
    :toDragPayload="(ast: Ast.Ast) => ast.serialize()"
    :fromDragPayload="Ast.deserialize"
    :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }">
      <NodeWidget
        :input="{ ...WidgetInput.FromAst(item), dynamicConfig: itemConfig, forcePort: true }"
        nest
      />
    </template>
  </ListWidget>
</template>
