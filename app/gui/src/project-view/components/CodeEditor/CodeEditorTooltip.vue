<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { NodeId } from '$/providers/openedProjects/graph'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import type { SuggestionDbStore } from '$/providers/openedProjects/suggestionDatabase'
import { computed } from 'vue'

const { nodeId, syntax, graphDb, suggestionDbStore } = defineProps<{
  nodeId: NodeId | undefined
  syntax: string
  graphDb: GraphDb
  suggestionDbStore: SuggestionDbStore
}>()

const { projectNames: projectNames } = useCurrentProject()

const expressionInfo = computed(() => nodeId && graphDb.getExpressionInfo(nodeId))
const typeName = computed(() => {
  const type = expressionInfo.value?.typeInfo?.primaryType
  if (type == null || projectNames.value == null) return 'Unknown'
  return projectNames.value.printProjectPath(type)
})
const executionTimeMs = computed(
  () =>
    expressionInfo.value?.profilingInfo[0] &&
    (expressionInfo.value.profilingInfo[0].ExecutionTime.nanoTime / 1_000_000).toFixed(3),
)
const method = computed(() => expressionInfo.value?.methodCall?.methodPointer)
const methodPath = computed(() => {
  if (method.value == null || projectNames.value == null) return 'Unknown'
  return projectNames.value.printProjectPath(method.value.definedOnType) + '.' + method.value.name
})
const group = computed(() => {
  const id = method.value && suggestionDbStore.entries.findByMethodPointer(method.value)
  if (id == null) return
  const suggestionEntry = suggestionDbStore.entries.get(id)
  if (!suggestionEntry) return
  const groupIndex = suggestionEntry.groupIndex
  if (groupIndex == null) return
  const group = suggestionDbStore.groups[groupIndex]
  if (!group) return
  return {
    name: `${group.project}.${group.name}`,
    color: group.color,
  }
})
</script>

<template>
  <div v-if="nodeId">AST ID: {{ nodeId }}</div>
  <div v-if="typeName">Type: {{ typeName }}</div>
  <div v-if="executionTimeMs != null">Execution Time: {{ executionTimeMs }}ms</div>
  <div>Syntax: {{ syntax }}</div>
  <div v-if="methodPath">Method: {{ methodPath }}</div>
  <div v-if="group" :style="{ color: group.color }">Group: {{ group.name }}</div>
</template>
