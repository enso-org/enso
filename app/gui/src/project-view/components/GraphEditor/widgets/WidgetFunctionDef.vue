<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ArgumentRow from '@/components/GraphEditor/widgets/WidgetFunctionDef/ArgumentRow.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { DocumentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, Ref } from 'vue'
import { newArgumentDefinition } from 'ydoc-shared/ast'
import { assertUnreachable } from 'ydoc-shared/util/assert'

const { input, onUpdate } = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const funcIcon = computed(() => {
  return input[FunctionInfoKey]?.docsData.value?.iconName ?? 'enso_logo'
})

function doEdit(editFn: (ast: Ast.MutableFunctionDef, edit: Ast.MutableModule) => void) {
  const edit = graph.startEdit()
  editFn(edit.getVersion(input.value), edit)
  onUpdate({ edit, directInteraction: true })
}

function handleAddItem() {
  if (input.editHandler?.addItem()) return
  doEdit((ast) => {
    ast.pushArgumentDefinitions(
      newArgumentDefinition(generateUniqueArgName(currentArgNames(ast), (i) => `arg${i}`)),
    )
  })
}

const currentArgNames = (ast: Ast.FunctionDef) =>
  new Set(ast.argumentDefinitions.map((a) => a.pattern.node.code()))

function generateUniqueArgName(
  existingNames: Set<string>,
  indexToName: (index: number) => string,
): string {
  for (let i = 1; i < 10000; i++) {
    const proposedName = indexToName(i)
    if (!existingNames.has(proposedName)) return proposedName
  }
  // Technically reachable, but not in any reasonable operation.
  // The loop exist condition is there only to prevent an infinite loop in case of a bug.
  assertUnreachable()
}

function handleRemove(index: number) {
  doEdit((ast) => ast.spliceArgumentDefinitions(index, 1))
}

function handleReorder(oldIndex: number, newIndex: number) {
  doEdit((ast) => ast.moveArgumentDefinitions(oldIndex, newIndex))
}

const funcNameInput = computed(() => {
  const nameAst = input.value.name
  const widgetInput = WidgetInput.FromAst(nameAst)
  widgetInput[DisplayIcon] = {
    icon: funcIcon.value,
    allowChoice: true,
    showContents: true,
  }

  const methodPointer = input[FunctionInfoKey]?.methodPointer
  if (methodPointer) {
    widgetInput[FunctionName] = {
      editableNameExpression: nameAst.externalId,
      methodPointer,
    }
  }
  return widgetInput
})

function handleRename(index: number, newName: Ast.Owned<Ast.MutableExpression>) {
  if (newName == null) return handleRemove(index)

  doEdit((ast, edit) => {
    const oldName = ast.argumentDefinitions[index]?.pattern.node.code()
    if (!oldName) return
    ast.visitRecursive((child) => {
      if (child instanceof Ast.Ident && child.code() === oldName)
        edit.replaceValue(child.id, newName)
    })
  })
}
</script>

<template>
  <div class="WidgetFunctionDef">
    <NodeWidget :input="funcNameInput" />
    <DraggableList
      axis="y"
      showHandles
      class="FunctionDefArguments"
      :items="input.value.argumentDefinitions"
      @addItem="handleAddItem"
      @remove="handleRemove"
      @reorder="handleReorder"
    >
      <template #default="{ item, index }">
        <ArgumentRow
          :definition="item"
          :onUpdate="onUpdate"
          @rename="handleRename(index, $event)"
        />
      </template>
    </DraggableList>
  </div>
</template>

<script lang="ts">
export const FunctionInfoKey: unique symbol = Symbol.for('WidgetInput:FunctionInfoKey')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionInfoKey]?: {
      methodPointer: MethodPointer
      docsData: Ref<DocumentationData | undefined>
    }
  }
}
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.FunctionDef),
  {
    priority: 999,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<style scoped>
.WidgetFunctionDef {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 4px;
}

.FunctionDefArguments {
  margin-left: 24px;
  gap: 4px;
}
</style>
