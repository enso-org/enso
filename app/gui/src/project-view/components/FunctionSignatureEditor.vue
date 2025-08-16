<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import WidgetTreeRoot from '@/components/GraphEditor/WidgetTreeRoot.vue'
import { providePopoverRoot } from '@/providers/popoverRoot'
import { applyWidgetUpdates, WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { documentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { useYText } from '@/util/crdt'
import { Ok } from '@/util/data/result'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, useTemplateRef } from 'vue'
import FormContainer from './FormContainer.vue'
import FormRow from './FormRow.vue'
import { FunctionName } from './GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from './GraphEditor/widgets/WidgetIcon.vue'

const { functionAst, methodPointer } = defineProps<{
  functionAst: Ast.FunctionDef
  methodPointer: MethodPointer | undefined
}>()

const rootElement = useTemplateRef('rootElement')
providePopoverRoot(rootElement)

const currentProject = useCurrentProject()

const docsString = useYText(() => functionAst.mutableDocumentationMarkdown())

const docsData = computed(() => {
  const definedIn = methodPointer?.module
  return (
    definedIn &&
    documentationData(
      docsString.value,
      definedIn.project,
      currentProject.ref.value?.suggestionDb.groups ?? [],
    )
  )
})

function handleWidgetUpdates(update: WidgetUpdate) {
  const graph = currentProject.ref.value?.graph
  if (graph) applyWidgetUpdates(update, graph)
  return Ok()
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

const funcArgsInput = computed(() => {
  const widgetInput = WidgetInput.FromAst(functionAst)
  return {
    input: widgetInput,
    externalId: functionAst.externalId,
    updateCallback: handleWidgetUpdates,
  }
})
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
        <!-- TODO: handle allowChoice to make item selection dropdown -->
        <WidgetTreeRoot class="widgetPill" v-bind="funcIconInput" />
      </FormRow>
      <FormRow>
        <template #label>Arguments (Name : Type = Default)</template>
        <!-- TODO: inline arg list and delete WidgetFunctionDef -->
        <WidgetTreeRoot class="widgetPill" v-bind="funcArgsInput" />
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
</style>
