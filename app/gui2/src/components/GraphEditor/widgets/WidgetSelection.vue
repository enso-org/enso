<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  functionCallConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { requiredImports, type RequiredImport } from '@/stores/graph/imports.ts'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { type SuggestionEntry } from '@/stores/suggestionDatabase/entry.ts'
import { ArgumentInfoKey } from '@/util/callTree'
import { qnLastSegment, tryQualifiedName, type Identifier } from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()

interface Tag {
  /** If not set, the label is same as expression */
  label?: string
  expression: string
  requiredImports?: RequiredImport[]
  parameters?: ArgumentWidgetConfiguration[]
}

function identToLabel(name: Identifier): string {
  return name.replaceAll('_', ' ')
}

function tagFromExpression(expression: string): Tag {
  const qn = tryQualifiedName(expression)
  if (!qn.ok) return { expression }
  const entry = suggestions.entries.getEntryByQualifiedName(qn.value)
  if (entry) return tagFromEntry(entry)
  return {
    label: identToLabel(qnLastSegment(qn.value)),
    expression: qn.value,
  }
}

function tagFromEntry(entry: SuggestionEntry): Tag {
  return {
    label: identToLabel(entry.name),
    expression:
      entry.selfType != null
        ? `_.${entry.name}`
        : entry.memberOf
        ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
        : entry.name,
    requiredImports: requiredImports(suggestions.entries, entry),
  }
}

const staticTags = computed<Tag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  return tags.map(tagFromExpression)
})

const dynamicTags = computed<Tag[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice') return []
  return config.values.map((value) => ({
    ...tagFromExpression(value.value),
    ...(value.label ? { label: value.label } : {}),
    parameters: value.parameters,
  }))
})

const tags = computed(() => (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value))
const tagLabels = computed(() => tags.value.map((tag) => tag.label ?? tag.expression))

const selectedIndex = ref<number>()
const selectedTag = computed(() =>
  selectedIndex.value != null ? tags.value[selectedIndex.value] : undefined,
)
const selectedExpression = computed(() => {
  if (selectedTag.value == null) return WidgetInput.valueRepr(props.input)
  return selectedTag.value.expression
})
const innerWidgetInput = computed(() => {
  if (selectedTag.value == null) return props.input
  const parameters = selectedTag.value.parameters
  if (!parameters) return props.input
  const config = functionCallConfiguration(parameters)
  return { ...props.input, dynamicConfig: config }
})
const showDropdownWidget = ref(false)

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  const edit = graph.astModule.edit()
  if (selectedTag.value?.requiredImports)
    graph.addMissingImports(edit, selectedTag.value.requiredImports)
  props.onUpdate({
    edit,
    portUpdate: {
      value: selectedExpression.value,
      origin: props.input.portId,
    },
  })
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 999,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Single_Choice') return Score.Perfect
    if (props.input[ArgumentInfoKey]?.info?.tagValues != null) return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetSelection" @pointerdown.stop="toggleDropdownWidget">
    <NodeWidget :input="innerWidgetInput" />
    <DropdownWidget
      v-if="showDropdownWidget"
      class="dropdownContainer"
      :color="'var(--node-color-primary)'"
      :values="tagLabels"
      :selectedValue="selectedExpression"
      @pointerdown.stop
      @click="selectedIndex = $event"
    />
  </div>
</template>

<style scoped>
.WidgetSelection {
  display: flex;
  flex-direction: row;
}
</style>
