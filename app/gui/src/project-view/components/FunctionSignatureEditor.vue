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
import { Ast } from '@/util/ast'
import { useYText } from '@/util/crdt'
import type { MethodPointer } from '@/util/methodPointer'
import { normalizeArgumentName } from '@/util/nameValidation'
import { Ok } from 'enso-common/src/utilities/data/result'
import { computed, useTemplateRef } from 'vue'
import { newArgumentDefinition, type Identifier } from 'ydoc-shared/ast'
import FormContainer from './FormContainer.vue'
import FormRow from './FormRow.vue'
import {
  generateUniqueName,
  renameArgumentInDefaultValue,
  replaceVariableUsages,
} from './GraphEditor/widgets/WidgetFunctionDef/argumentAst'
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
    const argIndex = ast.argumentDefinitions.length + 1
    const name = generateUniqueName((i) => `arg${i}` as Identifier, ast, [], argIndex)
    ast.pushArgumentDefinition(newArgumentDefinition(name))
  })
}

function doEdit(editFn: (ast: Ast.MutableFunctionDef, edit: Ast.MutableModule) => void) {
  module.value.edit((edit) => {
    editFn(edit.getVersion(functionAst), edit)
    return Ok()
  })
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
    replaceVariableUsages(edit, ast, oldNameString, newName)
  })
}

function preprocessName(index: number, value: string) {
  const oldName = functionAst.argumentDefinitions[index]?.pattern.node.code()
  return generateUniqueName(normalizeArgumentName(value), functionAst, oldName ? [oldName] : [])
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
              :preprocessName="(name) => preprocessName(index, name)"
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
