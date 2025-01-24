<script setup lang="ts">
import { WidgetInput } from '@/providers/widgetRegistry'
import { injectProjectNames } from '@/stores/projectNames'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { documentationData } from '@/stores/suggestionDatabase/documentation'
import { colorFromString } from '@/util/colors'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, ref, watchEffect } from 'vue'
import { FunctionDef } from 'ydoc-shared/ast'
import type * as Y from 'yjs'
import WidgetTreeRoot from './GraphEditor/WidgetTreeRoot.vue'
import { FunctionInfoKey } from './GraphEditor/widgets/WidgetFunctionDef.vue'

const suggestionDb = useSuggestionDbStore()
const projectNames = injectProjectNames()

const { functionAst, markdownDocs, methodPointer } = defineProps<{
  functionAst: FunctionDef
  markdownDocs: Y.Text | undefined
  methodPointer: MethodPointer | undefined
}>()

const docsString = ref<string>()

function updateDocs() {
  docsString.value = markdownDocs?.toJSON()
}

watchEffect((onCleanup) => {
  const localMarkdownDocs = markdownDocs
  if (localMarkdownDocs != null) {
    updateDocs()
    localMarkdownDocs.observe(updateDocs)
    onCleanup(() => localMarkdownDocs.unobserve(updateDocs))
  }
})

const docsData = computed(() => {
  const definedIn = methodPointer?.module
  return definedIn && documentationData(docsString.value, definedIn.project, suggestionDb.groups)
})

const treeRootInput = computed((): WidgetInput => {
  const input = WidgetInput.FromAst(functionAst)
  if (methodPointer) input[FunctionInfoKey] = { methodPointer, docsData }
  return input
})

const rootElement = ref<HTMLElement>()

function handleWidgetUpdates() {
  return true
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
</script>

<template>
  <div ref="rootElement" :style="rootStyle" class="FunctionSignatureEditor define-node-colors">
    <WidgetTreeRoot
      :externalId="functionAst.externalId"
      :input="treeRootInput"
      :rootElement="rootElement"
      :extended="true"
      :onUpdate="handleWidgetUpdates"
    />
  </div>
</template>

<style scoped>
.FunctionSignatureEditor {
  margin: 4px 8px;
  padding: 4px;

  /*
   * TODO: Add node coloring.
   * Function color cannot be inferred at the moment, as it depends on the output type.
   */

  border-radius: var(--node-border-radius);
  transition: background-color 0.2s ease;
  background-color: var(--color-node-background);
  box-sizing: border-box;
}
</style>
