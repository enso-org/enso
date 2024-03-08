<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { injectInteractionHandler } from '@/providers/interactionHandler'
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
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()
const interaction = injectInteractionHandler()
const widgetRoot = ref<HTMLElement>()

interface Tag {
  /** If not set, the label is same as expression */
  label?: string
  expression: string
  requiredImports?: RequiredImport[]
  parameters?: ArgumentWidgetConfiguration[]
  onClick?: () => void
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
      entry.selfType != null ? `_.${entry.name}`
      : entry.memberOf ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
      : entry.name,
    requiredImports: requiredImports(suggestions.entries, entry),
  }
}

function tagFromCustomItem(item: CustomDropdownItem): Tag {
  const expression = item.label
  return { expression, onClick: item.onClick }
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

const customTags = computed(() => props.input[CustomDropdownItemsKey]?.map(tagFromCustomItem) ?? [])
const tags = computed(() => {
  const standardTags = dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value
  return customTags.value.concat(standardTags)
})
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
  if (props.input.dynamicConfig == null) return props.input
  const config = props.input.dynamicConfig
  if (config.kind !== 'Single_Choice') return props.input
  return { ...props.input, dynamicConfig: singleChoiceConfiguration(config) }
})
const showDropdownWidget = ref(false)
interaction.setWhen(showDropdownWidget, {
  cancel: () => {
    showDropdownWidget.value = false
  },
  click: (e: PointerEvent) => {
    if (targetIsOutside(e, widgetRoot)) showDropdownWidget.value = false
    return false
  },
})

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

function onClick(index: number, keepOpen: boolean) {
  if (index < customTags.value.length) {
    customTags.value[index]!.onClick!()
  } else {
    selectedIndex.value = index
  }
  showDropdownWidget.value = keepOpen
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
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
    if (props.input[CustomDropdownItemsKey] != null) return Score.Perfect
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

/** Custom item added to dropdown. These items can’t be selected, but can be clicked. */
export interface CustomDropdownItem {
  /** Displayed label. */
  label: string
  /** Action to perform when clicked. */
  onClick: () => void
}

export const CustomDropdownItemsKey: unique symbol = Symbol('CustomDropdownItems')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [CustomDropdownItemsKey]?: CustomDropdownItem[]
  }
}
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
