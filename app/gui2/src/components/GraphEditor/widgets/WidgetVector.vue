<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract.ts'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const itemConfig = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor' ?
    props.input.dynamicConfig.item_editor
  : undefined,
)

const defaultItem = computed(() => {
  if (props.input.dynamicConfig?.kind === 'Vector_Editor') {
    return Ast.parse(props.input.dynamicConfig.item_default)
  } else {
    return Ast.Wildcard.new(MutableModule.Transient())
  }
})

const value = computed({
  get() {
    return props.input.value instanceof Ast.Vector ? [...props.input.value.values()] : []
  },
  set(value) {
    // This doesn't preserve AST identities, because the values are not `Ast.Owned`.
    // Getting/setting an Array is incompatible with ideal synchronization anyway;
    // `ListWidget` needs to operate on the `Ast.Vector` for edits to be merged as `Y.Array` operations.
    const tempModule = MutableModule.Transient()
    const newAst = Ast.Vector.new(
      tempModule,
      value.map((element) => tempModule.copy(element)),
    )
    props.onUpdate({
      portUpdate: { value: newAst, origin: props.input.portId },
    })
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 500,
  score: (props) =>
    props.input.dynamicConfig?.kind === 'Vector_Editor' ? Score.Perfect
    : props.input.expectedType?.startsWith('Standard.Base.Data.Vector.Vector') ? Score.Good
    : props.input.value instanceof Ast.Vector ? Score.Perfect
    : Score.Mismatch,
})
</script>

<template>
  <ListWidget
    v-model="value"
    :default="() => defaultItem"
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
