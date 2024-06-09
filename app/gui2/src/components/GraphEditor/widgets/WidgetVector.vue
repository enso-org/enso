<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const itemConfig = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor' ?
    props.input.dynamicConfig.item_editor
  : undefined,
)

const defaultItem = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor' ?
    Ast.parse(props.input.dynamicConfig.item_default)
  : DEFAULT_ITEM.value,
)

function newItem() {
  if (props.input.editHandler?.addItem()) return
  return defaultItem.value
}

const value = computed({
  get() {
    return props.input.value instanceof Ast.Vector ? [...props.input.value.values()] : []
  },
  set(value) {
    // This doesn't preserve AST identities, because the values are not `Ast.Owned`.
    // Getting/setting an Array is incompatible with ideal synchronization anyway;
    // `ListWidget` needs to operate on the `Ast.Vector` for edits to be merged as `Y.Array` operations.
    const newAst = Ast.Vector.build(value, (element, tempModule) => tempModule.copy(element))
    props.onUpdate({
      portUpdate: { value: newAst, origin: props.input.portId },
    })
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.placeholderOrAstMatcher(Ast.Vector),
  {
    priority: 500,
    score: (props) =>
      props.input.dynamicConfig?.kind === 'Vector_Editor' ? Score.Perfect
      : props.input.dynamicConfig?.kind === 'SomeOfFunctionCalls' ? Score.Perfect
      : props.input.value instanceof Ast.Vector ? Score.Good
      : props.input.expectedType?.startsWith('Standard.Base.Data.Vector.Vector') ? Score.Good
      : Score.Mismatch,
  },
  import.meta.hot,
)

const DEFAULT_ITEM = computed(() => Ast.Wildcard.new())
</script>

<template>
  <ListWidget
    v-model="value"
    :newItem="newItem"
    :getKey="(ast: Ast.Ast) => ast.id"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(ast: Ast.Ast) => ast.code()"
    :toDragPayload="(ast: Ast.Ast) => Ast.serialize(ast)"
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
