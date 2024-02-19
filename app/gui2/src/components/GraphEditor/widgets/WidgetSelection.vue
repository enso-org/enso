<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  functionCallConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { requiredImports, type ImportsForEntry } from '@/stores/graph/imports.ts'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { type SuggestionEntry, type SuggestionId } from '@/stores/suggestionDatabase/entry.ts'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract.ts'
import { ArgumentInfoKey } from '@/util/callTree'
import { asNot } from '@/util/data/types.ts'
import {
  qnLastSegment,
  tryQualifiedName,
  type IdentifierOrOperatorIdentifier,
} from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()

interface Tag {
  /** If not set, the label is same as expression */
  label?: string
  expression: string
  requiredImports?: ImportsForEntry
  parameters?: ArgumentWidgetConfiguration[]
}

function identToLabel(name: IdentifierOrOperatorIdentifier): string {
  return name.replaceAll('_', ' ')
}

function tagFromExpression(expression: string): Tag {
  const qn = tryQualifiedName(expression)
  if (!qn.ok) return { expression }
  const [entryId] = suggestions.entries.nameToId.lookup(qn.value)
  const entry = entryId != null ? suggestions.entries.get(entryId) : null
  if (entry) return tagFromEntry(entryId!, entry)
  return {
    label: identToLabel(qnLastSegment(qn.value)),
    expression: qn.value,
  }
}

function tagFromEntry(id: SuggestionId, entry: SuggestionEntry): Tag {
  return {
    label: identToLabel(entry.name),
    expression:
      entry.selfType != null
        ? `_.${entry.name}`
        : entry.memberOf
        ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
        : entry.name,
    requiredImports: { id, imports: requiredImports(suggestions.entries, entry) },
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

const removeSurroundingParens = (expr?: string) => expr?.trim().replaceAll(/(^[(])|([)]$)/g, '')

const selectedIndex = ref<number>()
const selectedTag = computed(() => {
  if (selectedIndex.value != null) {
    return tags.value[selectedIndex.value]
  } else {
    const currentExpression = removeSurroundingParens(WidgetInput.valueRepr(props.input))
    if (!currentExpression) return undefined
    // We need to find the tag that matches the (beginning of) current expression.
    // To prevent partial prefix matches, we arrange tags in reverse lexicographical order.
    const sortedTags = tags.value
      .map((tag, index) => [removeSurroundingParens(tag.expression), index] as [string, number])
      .sort(([a], [b]) => (a < b ? 1 : a > b ? -1 : 0))
    const [_, index] = sortedTags.find(([expr]) => currentExpression.startsWith(expr)) ?? []
    return index != null ? tags.value[index] : undefined
  }
})

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
  let edit: Ast.MutableModule | undefined
  // Unless import conflict resolution is needed, we use the selected expression as is.
  let value = selectedExpression.value
  if (selectedTag.value?.requiredImports) {
    edit = graph.startEdit()
    const conflicts = graph.addMissingImports(edit, [selectedTag.value.requiredImports])
    if (conflicts != null && conflicts.length > 0) {
      // Is there is a conflict, it would be a single one, because we only ask about a single entry.
      value = conflicts[0]?.fullyQualified
    }
  }
  props.onUpdate({
    edit,
    portUpdate: {
      value,
      origin: asNot<TokenId>(props.input.portId),
    },
  })
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 50,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Single_Choice') return Score.Perfect
    if (props.input[ArgumentInfoKey]?.info?.tagValues != null) return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetSelection" @pointerdown.stop="toggleDropdownWidget">
    <NodeWidget ref="childWidgetRef" :input="innerWidgetInput" />
    <SvgIcon name="arrow_right_head_only" class="arrow" />
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

.arrow {
  position: absolute;
  bottom: -6px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg);
}
</style>
