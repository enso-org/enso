<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { documentationData } from '$/providers/openedProjects/suggestionDatabase/documentation'
import {
  applyWidgetUpdates,
  WidgetInput,
  type WidgetUpdate,
} from '$/providers/openedProjects/widgetRegistry'
import WidgetTreeRoot from '@/components/GraphEditor/WidgetTreeRoot.vue'
import DraggableList from '@/components/widgets/DraggableList.vue'
import { providePopoverRoot } from '@/providers/popoverRoot'
import { syntheticPortId } from '@/providers/portInfo'
import { assertUnreachable } from '@/util/assert'
import { Ast } from '@/util/ast'
import { useYText } from '@/util/crdt'
import { Ok } from '@/util/data/result'
import type { MethodPointer } from '@/util/methodPointer'
import { computed, useTemplateRef } from 'vue'
import { newArgumentDefinition } from 'ydoc-shared/ast'
import FormContainer from './FormContainer.vue'
import FormRow from './FormRow.vue'
import { renameArgumentInDefaultValue } from './GraphEditor/widgets/WidgetFunctionDef/argumentAst'
import ArgumentRow from './GraphEditor/widgets/WidgetFunctionDef/ArgumentRow.vue'
import { FunctionName } from './GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from './GraphEditor/widgets/WidgetIcon.vue'

const { functionAst, methodPointer } = defineProps<{
  functionAst: Ast.FunctionDef
  methodPointer: MethodPointer | undefined
}>()

const rootElement = useTemplateRef('rootElement')
providePopoverRoot(rootElement)

const { suggestionDb, module } = useCurrentProject()

const docsString = useYText(() => functionAst.mutableDocumentationMarkdown())

const docsData = computed(() => {
  const definedIn = methodPointer?.module
  return (
    definedIn && documentationData(docsString.value, definedIn.project, suggestionDb.value.groups)
  )
})

function handleWidgetUpdates(update: WidgetUpdate) {
  return applyWidgetUpdates(update, module.value)
}

const funcNameInput = computed(() => {
  const nameAst = functionAst.name
  const widgetInput = WidgetInput.FromAst(nameAst)
  if (methodPointer) {
    widgetInput[FunctionName] = { editableNameExpression: nameAst.externalId, methodPointer }
  }
  return { input: widgetInput, externalId: nameAst.externalId, updateCallback: handleWidgetUpdates }
})

const funcIconInput = computed(() => {
  const icon = docsData.value?.iconName ?? 'enso_logo'
  const nameAst = functionAst.name
  const widgetInput = WidgetInput.FromAst(nameAst)
  widgetInput[DisplayIcon] = { icon, allowChoice: true, showContents: false }
  return { input: widgetInput, externalId: nameAst.externalId, updateCallback: handleWidgetUpdates }
})

// === Editing arguments ===

const functionArgs = computed(() => functionAst.argumentDefinitions)

function handleAddItem() {
  doEdit((ast) => {
    ast.pushArgumentDefinition(
      newArgumentDefinition(generateUniqueArgName(currentArgNames(ast), (i) => `arg${i}`)),
    )
  })
}

function doEdit(editFn: (ast: Ast.MutableFunctionDef, edit: Ast.MutableModule) => void) {
  module.value.edit((edit) => {
    editFn(edit.getVersion(functionAst), edit)
    return Ok()
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

function handleRename(index: number, newName: Ast.Owned<Ast.MutableExpression>) {
  if (newName == null) return handleRemove(index)

  doEdit((ast, edit) => {
    const definition = ast.argumentDefinitions[index]
    if (!definition) return
    const oldNameString = definition.pattern.node.code()
    const newNameString = newName.code()
    if (newNameString == oldNameString) return
    renameArgumentInDefaultValue(definition, edit, newNameString)
    Ast.visitRecursive(ast, (child) => {
      if (child instanceof Ast.Ident && child.token.code() === oldNameString)
        edit.replaceValue(child.id, newName)
    })
  })
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

/** Stable identifier for each argument, keeps unaffected widgets from rerendering. */
function makeArgRowId(arg: Ast.ArgumentDefinition<Ast.ConcreteRefs>) {
  return arg.pattern.node.externalId
}
</script>

<template>
  <div ref="rootElement" class="FunctionSignatureEditor define-node-colors">
    <FormContainer>
      <FormRow>
        <template #label>User-Defined Component Name</template>
        <WidgetTreeRoot v-bind="funcNameInput" />
      </FormRow>
      <FormRow inline>
        <template #label>Icon</template>
        <!-- TODO: handle WidgetIcon's allowChoice to make icon selection dropdown -->
        <WidgetTreeRoot class="widgetPill" v-bind="funcIconInput" />
      </FormRow>
      <FormRow>
        <template #label>Arguments (Name : Type = Default)</template>
        <DraggableList
          axis="y"
          showHandles
          horizontalScroll
          class="ArgumentList widgetPill"
          :items="functionArgs"
          :getKey="makeArgRowId"
          @addItem="handleAddItem"
          @remove="handleRemove"
          @reorder="handleReorder"
        >
          <template #default="{ item, index }">
            <ArgumentRow
              :root="rootElement"
              :portIdBase="syntheticPortId(functionAst.id, `argRow:${index}`)"
              :definition="item"
              :updateCallback="handleWidgetUpdates"
              @rename="handleRename(index, $event)"
              @updateType="handleUpdateType(index, $event)"
              @updateDefault="handleUpdateDefault(index, $event)"
            />
          </template>
        </DraggableList>
      </FormRow>
      <FormRow>
        <template #label>Documentation</template>
      </FormRow>
    </FormContainer>
  </div>
</template>

<style scoped>
.FunctionSignatureEditor {
  --node-group-color: white;
  --color-node-text: black;
  --node-port-shadow: inset 0 0 0 1px black;
  --dropdown-item-hover-bg: var(--color-menu-entry-hover-bg);
  --dropdown-item-selected-bg: var(--color-menu-entry-selected-bg);
}

.ArgumentList {
  padding: 8px;
  gap: 4px;
}
</style>
