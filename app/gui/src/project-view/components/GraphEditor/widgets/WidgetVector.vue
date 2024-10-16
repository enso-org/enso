<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import type { PortId } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { computed, shallowRef, toRef, toValue, watchEffect, type WatchSource } from 'vue'
import { isAstId } from 'ydoc-shared/ast'

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

function useChildEditForwarding(input: WatchSource<Ast.Ast | unknown>) {
  let editStarted = false
  const childEdit = shallowRef<{ origin: PortId; editedValue: Ast.Owned | string }>()

  watchEffect(() => {
    if (!editStarted && !childEdit.value) return
    const inputValue = toValue(input)
    if (!(inputValue instanceof Ast.Ast)) return
    const editedAst = Ast.copyIntoNewModule(inputValue)
    if (childEdit.value) {
      const module = editedAst.module
      const origin = childEdit.value.origin
      const ast = isAstId(origin) ? module.tryGet(origin) : undefined
      if (ast) {
        const replacement = childEdit.value.editedValue
        ast.replace(typeof replacement === 'string' ? Ast.parse(replacement, module) : replacement)
      }
    }
    editHandler.edit(editedAst)
    editStarted = true
  })

  return {
    childEnded: (origin: PortId) => {
      if (childEdit.value?.origin === origin) childEdit.value = undefined
    },
    edit: (origin: PortId, value: Ast.Owned | string) => {
      // The ID is used to locate a subtree; if the port isn't identified by an AstId, the lookup will simply fail.
      childEdit.value = { origin, editedValue: value }
    },
  }
}
const { childEnded, edit } = useChildEditForwarding(toRef(props.input, 'value'))

const editHandler = WidgetEditHandler.New('WidgetVector', props.input, {
  cancel: () => {},
  end: () => {},
  childEnded,
  edit,
})

function itemInput(ast: Ast.Ast): WidgetInput {
  return {
    ...WidgetInput.FromAst(ast),
    dynamicConfig: itemConfig.value,
    forcePort: true,
    editHandler,
  }
}
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
      <NodeWidget :input="itemInput(item)" nest />
    </template>
  </ListWidget>
</template>
