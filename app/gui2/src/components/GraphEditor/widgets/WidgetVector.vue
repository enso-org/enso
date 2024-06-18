<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { computed, shallowRef, triggerRef, watchEffect } from 'vue'

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

function useChildEditForwarding(widgetId: string) {
  const editHandler = WidgetEditHandler.New(widgetId, props.input, {
    cancel: () => (itemEdits.value = new Map()),
    end: () => (itemEdits.value = new Map()),
  })

  const itemEdits = shallowRef(new Map<Ast.AstId, Ast.Owned | string>())
  const updateEdits = (f: (itemEdits: Map<Ast.AstId, Ast.Owned | string>) => void) => {
    f(itemEdits.value)
    triggerRef(itemEdits)
  }

  function childEditHandler(widgetInput: WidgetInput, astId: Ast.AstId) {
    return editHandler.child(`${widgetId}.Child`, widgetInput, {
      cancel: () => updateEdits((itemEdits) => itemEdits.delete(astId)),
      end: () => updateEdits((itemEdits) => itemEdits.delete(astId)),
      edit: (_origin, value) => {
        updateEdits((itemEdits) => itemEdits.set(astId, value))
        return false
      },
    })
  }

  let editStarted = false
  watchEffect(() => {
    const inputValue = props.input.value
    if (!(inputValue instanceof Ast.Ast)) return
    if (!editStarted && itemEdits.value.size === 0) return
    const editedAst = Ast.copyIntoNewModule(inputValue)
    const module = editedAst.module
    for (const [id, replacement] of itemEdits.value) {
      const ast = module.tryGet(id)
      if (!ast) {
        continue
      }
      ast.replace(typeof replacement === 'string' ? Ast.parse(replacement, module) : replacement)
    }
    editHandler.edit(editedAst)
    editStarted = true
  })

  return { childEditHandler }
}

const { childEditHandler } = useChildEditForwarding('WidgetVector')

function itemInput(ast: Ast.Ast): WidgetInput {
  const baseInput = WidgetInput.FromAst(ast)
  return {
    ...baseInput,
    dynamicConfig: itemConfig.value,
    forcePort: true,
    editHandler: childEditHandler(baseInput, ast.id),
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
