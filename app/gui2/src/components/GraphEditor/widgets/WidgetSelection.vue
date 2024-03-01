<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  singleChoiceConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { requiredImports, type RequiredImport } from '@/stores/graph/imports.ts'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import {
  type SuggestionEntry,
  type SuggestionEntryArgument,
} from '@/stores/suggestionDatabase/entry.ts'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract.ts'
import { ArgumentInfoKey } from '@/util/callTree'
import { arrayEquals } from '@/util/data/array'
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
  requiredImports?: RequiredImport[]
  parameters?: ArgumentWidgetConfiguration[]
}

function tagFromExpression(expression: string): Tag {
  const qn = tryQualifiedName(expression)
  if (!qn.ok) return { expression }
  const entry = suggestions.entries.getEntryByQualifiedName(qn.value)
  if (entry) return tagFromEntry(entry)
  return {
    label: qnLastSegment(qn.value),
    expression: qn.value,
  }
}

function tagFromEntry(entry: SuggestionEntry): Tag {
  return {
    label: entry.name,
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
    parameters: value.parameters,
  }))
})

const tags = computed(() => (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value))
const tagLabels = computed(() => tags.value.map((tag) => tag.label ?? tag.expression))

const removeSurroundingParens = (expr?: string) => expr?.trim().replaceAll(/(^[(])|([)]$)/g, '')

const selectedIndex = ref<number>()
// When the input changes, we need to reset the selected index.
watch(
  () => props.input.value,
  () => (selectedIndex.value = undefined),
)
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

const selectedLabel = computed(() => {
  return selectedTag.value?.label
})
const innerWidgetInput = computed(() => {
  if (props.input.dynamicConfig == null) return props.input
  const config = props.input.dynamicConfig
  if (config.kind !== 'Single_Choice') return props.input
  return { ...props.input, dynamicConfig: singleChoiceConfiguration(config) }
})
const showDropdownWidget = ref(false)

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

function onClick(index: number, keepOpen: boolean) {
  selectedIndex.value = index
  showDropdownWidget.value = keepOpen
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  let edit: Ast.MutableModule | undefined
  if (selectedTag.value?.requiredImports) {
    edit = graph.startEdit()
    graph.addMissingImports(edit, selectedTag.value.requiredImports)
  }
  props.onUpdate({
    edit,
    portUpdate: {
      value: selectedTag.value?.expression,
      origin: asNot<TokenId>(props.input.portId),
    },
  })
})

const isHovered = ref(false)
</script>

<script lang="ts">
function hasBooleanTagValues(parameter: SuggestionEntryArgument): boolean {
  if (parameter.tagValues == null) return false
  return arrayEquals(Array.from(parameter.tagValues).sort(), [
    'Standard.Base.Data.Boolean.Boolean.False',
    'Standard.Base.Data.Boolean.Boolean.True',
  ])
}

export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 50,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Single_Choice') return Score.Perfect
    // Boolean arguments also have tag values, but the checkbox widget should handle them.
    if (
      props.input[ArgumentInfoKey]?.info?.tagValues != null &&
      !hasBooleanTagValues(props.input[ArgumentInfoKey].info)
    )
      return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <!-- See comment in GraphNode next to dragPointer definition about stopping pointerdown and pointerup -->
  <div
    class="WidgetSelection"
    @pointerdown.stop
    @pointerup.stop
    @click.stop="toggleDropdownWidget"
    @pointerover="isHovered = true"
    @pointerout="isHovered = false"
  >
    <NodeWidget ref="childWidgetRef" :input="innerWidgetInput" />
    <SvgIcon v-if="isHovered" name="arrow_right_head_only" class="arrow" />
    <DropdownWidget
      v-if="showDropdownWidget"
      class="dropdownContainer"
      :color="'var(--node-color-primary)'"
      :values="tagLabels"
      :selectedValue="selectedLabel"
      @click="onClick"
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
  bottom: -7px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  opacity: 0.5;
}
</style>
