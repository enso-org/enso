<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
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
import { targetIsOutside } from '@/util/autoBlur'
import { ArgumentInfoKey } from '@/util/callTree'
import { arrayEquals } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()
const interaction = injectInteractionHandler()
const widgetRoot = ref<HTMLElement>()

const filter = ref<string>()
const innerWidgetInteraction = ref<Interaction>()
const isHovered = ref(false)

interface Tag {
  /** If not set, the label is same as expression */
  label?: string
  expression: string
  requiredImports?: RequiredImport[]
  parameters?: ArgumentWidgetConfiguration[]
}

function tagFromExpression(expression: string, label?: Opt<string>): Tag {
  const qn = tryQualifiedName(expression)
  if (!qn.ok) return { expression, ...(label ? { label } : {}) }
  const entry = suggestions.entries.getEntryByQualifiedName(qn.value)
  if (entry) {
    const tag = tagFromEntry(entry)
    return label ? { ...tag, label: label } : tag
  }
  return {
    label: label ?? qnLastSegment(qn.value),
    expression: qn.value,
  }
}

function tagFromEntry(entry: SuggestionEntry): Tag {
  return {
    label: entry.name,
    expression:
      entry.selfType != null ? `_.${entry.name}`
      : entry.memberOf ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
      : entry.name,
    requiredImports: requiredImports(suggestions.entries, entry),
  }
}

function isFilteredIn(tag: Tag): boolean {
  const pattern = filter.value
  if (!pattern) return true
  const textLiteralPattern = /^(['"])(.*)\1$/.exec(pattern)
  if (textLiteralPattern) {
    const [, sep, pattern] = textLiteralPattern
    return tag.expression.startsWith(`${sep}${pattern}`) && tag.expression.endsWith(sep!)
  } else {
    return tag.expression.startsWith(pattern)
  }
}

const staticTags = computed<Tag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  return tags.map((t) => tagFromExpression(t))
})

const dynamicTags = computed<Tag[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice') return []
  return config.values.map((value) => ({
    ...tagFromExpression(value.value, value.label),
    parameters: value.parameters,
  }))
})

const tags = computed(() =>
  (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value).filter(isFilteredIn),
)
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
      .sort(([a], [b]) =>
        a < b ? 1
        : a > b ? -1
        : 0,
      )
    const [_, index] = sortedTags.find(([expr]) => currentExpression.startsWith(expr)) ?? []
    return index != null ? tags.value[index] : undefined
  }
})

const selectedLabel = computed(() => {
  return selectedTag.value?.label
})
const innerWidgetInput = computed(() => {
  const newInput: WidgetInput = {
    ...props.input,
    editing: {
      onStarted: (interaction) => {
        innerWidgetInteraction.value = interaction
        showDropdownWidget.value = true
      },
      onEdited: (value) => {
        filter.value = value instanceof Ast.Ast ? value.code() : value
      },
      onFinished: () => {
        showDropdownWidget.value = false
      },
    },
  }
  if (props.input.dynamicConfig == null) return newInput
  const config = props.input.dynamicConfig
  if (config.kind !== 'Single_Choice') return newInput
  newInput.dynamicConfig = singleChoiceConfiguration(config)
  return newInput
})
const showDropdownWidget = ref(false)
// interaction.setWhen(showDropdownWidget, {
//   cancel: () => {
//     showDropdownWidget.value = false
//   },
//   click: (e: PointerEvent) => {
//     if (targetIsOutside(e, widgetRoot)) showDropdownWidget.value = false
//     return false
//   },
// })

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

function onClick(index: number, keepOpen: boolean) {
  selectedIndex.value = index
  showDropdownWidget.value = keepOpen
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  console.log('Set drop-down value', innerWidgetInteraction.value)
  if (innerWidgetInteraction.value != null) {
    interaction.end(innerWidgetInteraction.value)
  }
  let edit: Ast.MutableModule | undefined
  // Unless import conflict resolution is needed, we use the selected expression as is.
  let value = selectedTag.value?.expression
  if (selectedTag.value?.requiredImports) {
    edit = graph.startEdit()
    const conflicts = graph.addMissingImports(edit, selectedTag.value.requiredImports)
    if (conflicts != null && conflicts.length > 0) {
      // Is there is a conflict, it would be a single one, because we only ask about a single entry.
      value = conflicts[0]?.fullyQualified
    }
  }
  props.onUpdate({ edit, portUpdate: { value, origin: props.input.portId } })
})
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
    ref="widgetRoot"
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
