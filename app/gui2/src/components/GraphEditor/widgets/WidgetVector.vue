<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast, RawAst } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

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
    if (props.input.ast == null) return []
    return Array.from(props.input.ast.children()).filter(
      (child): child is Ast.Ast => child instanceof Ast.Ast,
    )
  },
  set(value) {
    const newCode = `[${value.map((item) => item.code()).join(', ')}]`
    props.onUpdate(newCode, props.input.portId)
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 1000,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Vector_Editor') return Score.Perfect
    else if (
      props.input[ArgumentInfoKey]?.info?.reprType.startsWith('Standard.Base.Data.Vector.Vector')
    )
      return Score.Good
    else
      return props.input.ast?.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="() => defaultItem"
    :getKey="(ast: Ast.Ast) => ast.exprId"
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
      />
    </template>
  </ListWidget>
</template>
