<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import type { PortId } from '@/providers/portInfo'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed, shallowRef, toRef, toValue, watchEffect, type WatchSource } from 'vue'
import { isAstId, MutableModule } from 'ydoc-shared/ast'

const props = defineProps(widgetProps(widgetDefinition))
const project = useCurrentProject()
const tree = injectWidgetTree()

function doEdit(editFn: (ast: Ast.MutableVector) => void) {
  project.module.value.edit((edit) => {
    if (props.input.value instanceof Ast.Vector) {
      editFn(edit.getVersion(props.input.value))
      return props.updateCallback({ edit, directInteraction: true })
    } else {
      const value = Ast.Vector.new(MutableModule.Transient(), [])
      editFn(value)
      return props.updateCallback({
        edit,
        portUpdate: { value, origin: props.input.portId },
        directInteraction: true,
      })
    }
  })
}

const itemConfig = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor' ?
    props.input.dynamicConfig.item_editor
  : undefined,
)

const defaultItem = computed(() =>
  props.input.dynamicConfig?.kind === 'Vector_Editor' ?
    (Ast.parseExpression(props.input.dynamicConfig.item_default) ?? DEFAULT_ITEM.value)
  : DEFAULT_ITEM.value,
)

function handleAddItem() {
  if (props.input.editHandler?.addItem()) return
  doEdit((ast) => ast.push(defaultItem.value))
}

function handleRemove(index: number) {
  doEdit((ast) => ast.splice(index, 1))
}

function handleReorder(oldIndex: number, newIndex: number) {
  doEdit((ast) => ast.move(oldIndex, newIndex))
}

function handleDropInsert(index: number, payload: string) {
  const expr = Ast.deserializeExpression(payload)
  if (!expr) return
  doEdit((ast) => ast.splice(index, 0, expr))
}

const value = computed({
  get() {
    return props.input.value instanceof Ast.Vector ? [...props.input.value.values()] : []
  },
  set(value) {
    // This doesn't preserve AST identities, because the values are not `Ast.Owned`.
    // Getting/setting an Array is incompatible with ideal synchronization anyway;
    // `DraggableList` needs to operate on the `Ast.Vector` for edits to be merged as `Y.Array` operations.
    const newAst = Ast.Vector.build(value, (element, tempModule) => tempModule.copy(element))
    props.updateCallback({
      portUpdate: { value: newAst, origin: props.input.portId },
      directInteraction: true,
    })
  },
})

const navigator = injectGraphNavigator(true)

function useChildEditForwarding(input: WatchSource<Ast.Expression | unknown>) {
  let editStarted = false
  const childEdit = shallowRef<{
    origin: PortId
    editedValue: Ast.Owned<Ast.MutableExpression> | string
  }>()

  watchEffect(() => {
    if (!editStarted && !childEdit.value) return
    const inputValue = toValue(input)
    if (!(inputValue instanceof Ast.Ast)) return
    const editedAst = Ast.copyIntoNewModule(inputValue as Ast.Expression)
    if (childEdit.value) {
      const module = editedAst.module
      const origin = childEdit.value.origin
      const ast = isAstId(origin) ? module.tryGet(origin) : undefined
      if (ast) {
        const replacement = childEdit.value.editedValue
        ast.replace(
          typeof replacement === 'string' ? Ast.parseExpression(replacement, module)! : replacement,
        )
      }
    }
    editHandler.value.edit(editedAst)
    editStarted = true
  })

  return {
    childEnded: (origin: PortId) => {
      if (childEdit.value?.origin === origin) childEdit.value = undefined
    },
    edit: (origin: PortId, value: Ast.Owned<Ast.MutableExpression> | string) => {
      // The ID is used to locate a subtree; if the port isn't identified by an AstId, the lookup will simply fail.
      childEdit.value = { origin, editedValue: value }
    },
  }
}
const { childEnded, edit } = useChildEditForwarding(toRef(props.input, 'value'))

const editHandler = WidgetEditHandler.New(props, {
  cancel: () => {},
  end: () => {},
  childEnded,
  edit,
})

function itemInput(ast: Ast.Expression): WidgetInput {
  return {
    ...WidgetInput.FromAst(ast),
    dynamicConfig: itemConfig.value,
    forcePort: true,
    editHandler: editHandler.value,
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
      : props.input.dynamicConfig?.kind === 'Pending' ? Score.Mismatch
      : props.input.value instanceof Ast.Vector ? Score.Good
      : props.input.expectedType?.startsWith('Standard.Base.Data.Vector.Vector') ? Score.Good
      : Score.Mismatch,
  },
  import.meta.hot,
)

const DEFAULT_ITEM = computed(() => Ast.Wildcard.new())
</script>

<template>
  <div class="WidgetVector widgetParent">
    <span class="token widgetSingleLine widgetApplyPadding">[</span>
    <DraggableList
      :items="value"
      axis="x"
      :showHandles="tree.extended"
      :getKey="(ast) => ast.id"
      dragMimeType="application/x-enso-ast-node"
      :toPlainText="Ast.serializeExpression"
      :toDragPayload="Ast.serializeExpression"
      :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
      @addItem="handleAddItem"
      @remove="handleRemove"
      @reorder="handleReorder"
      @dropInsert="handleDropInsert"
    >
      <template #default="{ item }">
        <NodeWidget :input="itemInput(item)" nest />
      </template>
      <template #separator>
        <div class="token widgetSingleLine widgetApplyPadding">,&nbsp;</div>
      </template>
    </DraggableList>
    <span class="token widgetSingleLine widgetApplyPadding">]</span>
  </div>
</template>
<style scoped>
.token {
  opacity: 0.33;
  user-select: none;
}
</style>
