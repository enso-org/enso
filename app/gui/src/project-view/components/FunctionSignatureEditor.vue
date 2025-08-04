<script setup lang="ts">
import {
  useGraphStore,
  useProjectNames,
  useSuggestionDbStore,
} from '$/components/WithCurrentProject.vue'
import WidgetTreeRoot from '@/components/GraphEditor/WidgetTreeRoot.vue'
import { FunctionInfoKey } from '@/components/GraphEditor/widgets/WidgetFunctionDef.vue'
import { providePopoverRoot } from '@/providers/popoverRoot'
import { applyWidgetUpdates, WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { emptyPrimaryApplication } from '@/stores/graph/graphDatabase'
import { documentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { colorFromString } from '@/util/colors'
import { useYText } from '@/util/crdt'
import { Ok } from '@/util/data/result'
import { type MethodPointer } from '@/util/methodPointer'
import { useFocusWithin } from '@vueuse/core'
import { computed, useTemplateRef } from 'vue'

const suggestionDb = useSuggestionDbStore()
const projectNames = useProjectNames()

const { functionAst, methodPointer } = defineProps<{
  functionAst: Ast.FunctionDef
  methodPointer: MethodPointer | undefined
}>()

const docsString = useYText(() => functionAst.mutableDocumentationMarkdown())

const docsData = computed(() => {
  const definedIn = methodPointer?.module
  return definedIn && documentationData(docsString.value, definedIn.project, suggestionDb.groups)
})

const treeRootInput = computed((): WidgetInput => {
  const input = WidgetInput.FromAst(functionAst)
  if (methodPointer) input[FunctionInfoKey] = { methodPointer, docsData }
  return input
})

const rootElement = useTemplateRef('rootElement')
const { focused } = useFocusWithin(rootElement)
providePopoverRoot(rootElement)

const graph = useGraphStore()

function handleWidgetUpdates(update: WidgetUpdate) {
  applyWidgetUpdates(update, graph)
  return Ok()
}

const groupBasedColor = computed(() => {
  const groupIndex = docsData.value?.groupIndex
  return groupIndex != null ? suggestionDb.groups[groupIndex]?.color : undefined
})

const returnTypeBasedColor = computed(() => {
  if (!methodPointer) return
  const suggestionId = suggestionDb.entries.findByMethodPointer(methodPointer)
  if (suggestionId == null) return
  const entry = suggestionDb.entries.get(suggestionId)
  if (!entry) return
  return colorFromString(entry.returnType(projectNames))
})

const rootStyle = computed(() => {
  return {
    '--node-group-color':
      groupBasedColor.value ?? returnTypeBasedColor.value ?? 'var(--group-color-fallback)',
  }
})

// We surely don’t have primary application for the function definition.
const primaryApplication = emptyPrimaryApplication()
</script>

<template>
  <div
    ref="rootElement"
    :style="rootStyle"
    class="FunctionSignatureEditor define-node-colors"
    :class="{ selected: focused }"
  >
    <WidgetTreeRoot
      :selected="focused"
      :externalId="functionAst.externalId"
      :input="treeRootInput"
      :primaryApplication="primaryApplication"
      :rootElement="rootElement"
      :extended="true"
      :updateCallback="handleWidgetUpdates"
    />
  </div>
</template>

<style scoped>
.FunctionSignatureEditor {
  padding: 4px;

  /*
   * TODO: Add node coloring.
   * Function color cannot be inferred at the moment, as it depends on the output type.
   */

  border-radius: var(--node-border-radius);
  transition: background-color 0.2s ease;
  background-color: var(--color-node-background);
}
</style>
