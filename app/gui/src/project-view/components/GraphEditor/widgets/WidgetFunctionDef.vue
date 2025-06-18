<script setup lang="ts">
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ArgumentRow from '@/components/GraphEditor/widgets/WidgetFunctionDef/ArgumentRow.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { syntheticPortId } from '@/providers/portInfo'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { DocumentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, Ref } from 'vue'
import { newArgumentDefinition } from 'ydoc-shared/ast'
import { assertUnreachable } from 'ydoc-shared/util/assert'
import { renameArgumentInDefaultValue } from './WidgetFunctionDef/argumentAst'

const { input, onUpdate } = defineProps(widgetProps(widgetDefinition))
const openedProject = injectCurrentProject().ref
const tree = injectWidgetTree()

const funcIcon = computed(() => {
  return input[FunctionInfoKey]?.docsData.value?.iconName ?? 'enso_logo'
})

function doEdit(editFn: (ast: Ast.MutableFunctionDef, edit: Ast.MutableModule) => void) {
  const edit = openedProject.value?.graph.startEdit()
  if (!edit) return
  editFn(edit.getVersion(input.value), edit)
  onUpdate({ edit, directInteraction: true })
}

function handleAddItem() {
  if (input.editHandler?.addItem()) return
  doEdit((ast) => {
    ast.pushArgumentDefinition(
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

function handleUpdateType(index: number, typeExpr: Ast.Owned<Ast.MutableExpression> | undefined) {
  doEdit((ast) => ast.setArgumentType(index, typeExpr))
}

function handleUpdateDefault(
  index: number,
  typeExpr: Ast.Owned<Ast.MutableExpression> | undefined,
) {
  doEdit((ast) => ast.setArgumentDefault(index, typeExpr))
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
    const definition = ast.argumentDefinitions[index]
    if (!definition) return
    const oldNameString = definition.pattern.node.code()
    const newNameString = newName.code()
    if (newNameString == oldNameString) return
    renameArgumentInDefaultValue(definition, edit, newNameString)
    ast.visitRecursive((child) => {
      if (child instanceof Ast.Ident && child.token.code() === oldNameString)
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
          :root="tree.rootElement"
          :portIdBase="syntheticPortId(input.portId, `argRow:${index}`)"
          :definition="item"
          :onUpdate="onUpdate"
          @rename="handleRename(index, $event)"
          @updateType="handleUpdateType(index, $event)"
          @updateDefault="handleUpdateDefault(index, $event)"
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
