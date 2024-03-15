<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import {
  singleChoiceConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
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
import { computed, ref, watch, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()
const widgetRoot = ref<HTMLElement>()
const dropdownElement = ref<ComponentInstance<typeof DropdownWidget>>()

const editedValue = ref<Ast.Ast | string | undefined>()
const isHovered = ref(false)

// interface Tag {
//   /** If not set, the label is same as expression */
//   label?: string
//   expression: string
//   requiredImports?: RequiredImport[]
//   parameters?: ArgumentWidgetConfiguration[]
// }

class Tag {
  private cachedExpressionAst: Ast.Ast | undefined

  constructor(
    readonly expression: string,
    private explicitLabel?: Opt<string>,
    readonly requiredImports?: RequiredImport[],
    public parameters?: ArgumentWidgetConfiguration[],
  ) {}

  static FromExpression(expression: string, label?: Opt<string>): Tag {
    const qn = tryQualifiedName(expression)
    if (!qn.ok) return new Tag(expression, label)
    const entry = suggestions.entries.getEntryByQualifiedName(qn.value)
    if (entry) return Tag.FromEntry(entry, label)
    return new Tag(qn.value, label ?? qnLastSegment(qn.value))
  }

  static FromEntry(entry: SuggestionEntry, label?: Opt<string>): Tag {
    const expression =
      entry.selfType != null ? `_.${entry.name}`
      : entry.memberOf ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
      : entry.name
    return new Tag(expression, label ?? entry.name, requiredImports(suggestions.entries, entry))
  }

  get label() {
    return this.explicitLabel ?? this.expression
  }

  get expressionAst() {
    if (this.cachedExpressionAst == null) {
      this.cachedExpressionAst = Ast.parse(this.expression)
    }
    return this.cachedExpressionAst
  }

  isFilteredIn(): boolean {
    if (editedTextLiteralValuePattern.value) {
      return (
        this.expressionAst instanceof Ast.TextLiteral &&
        this.expressionAst.rawTextContent.startsWith(editedTextLiteralValuePattern.value)
      )
    } else if (editedValuePattern.value) {
      return this.expression.startsWith(editedValuePattern.value)
    } else {
      return true
    }
  }
}

class CustomTag {
  constructor(
    readonly label: string,
    readonly onClick: () => void,
  ) {}

  static FromItem(item: CustomDropdownItem): CustomTag {
    return new CustomTag(item.label, item.onClick)
  }

  isFilteredIn(): boolean {
    // User writing something in inner inputs wants to create an expression, so custom
    // tags are hidden in that case.
    return !(editedTextLiteralValuePattern.value || editedValuePattern.value)
  }
}

const editedValuePattern = computed(() =>
  editedValue.value instanceof Ast.Ast ? editedValue.value.code() : editedValue.value,
)
const editedTextLiteralValuePattern = computed(() => {
  const editedAst =
    typeof editedValue.value === 'string' ? Ast.parse(editedValue.value) : editedValue.value
  return editedAst instanceof Ast.TextLiteral ? editedAst.rawTextContent : undefined
})

const staticTags = computed<Tag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  return tags.map((t) => Tag.FromExpression(t))
})

const dynamicTags = computed<Tag[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice') return []

  return config.values.map((value) => {
    const tag = Tag.FromExpression(value.value, value.label)
    tag.parameters = value.parameters
    return tag
  })
})

const customTags = computed(
  () => props.input[CustomDropdownItemsKey]?.map(CustomTag.FromItem) ?? [],
)
const tags = computed(() => {
  const standardTags = dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value
  return [...customTags.value, ...standardTags]
})
const filteredTags = computed(() => {
  return Array.from(tags.value, (tag, index) => ({
    tag,
    index,
  })).filter(({ tag }) => tag.isFilteredIn())
})
const filteredTagLabels = computed(() => filteredTags.value.map(({ tag }) => tag.label))

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
      .filter((tag) => tag instanceof Tag)
      .map(
        (tag, index) =>
          [removeSurroundingParens((tag as Tag).expression), index] as [string, number],
      )
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
const innerWidgetInput = computed<WidgetInput>(() => {
  const dynamicConfig =
    props.input.dynamicConfig?.kind === 'Single_Choice' ?
      singleChoiceConfiguration(props.input.dynamicConfig)
    : undefined
  return {
    ...props.input,
    editHandler: dropDownInteraction,
    dynamicConfig,
  }
})
const dropdownVisible = ref(false)
const dropDownInteraction = WidgetEditHandler.New(props.input, {
  cancel: () => {
    dropdownVisible.value = false
  },
  click: (e, _, childHandler) => {
    if (targetIsOutside(e, unrefElement(dropdownElement))) {
      if (childHandler) return childHandler()
      else dropdownVisible.value = false
    }
    return false
  },
  start: () => {
    dropdownVisible.value = true
    editedValue.value = undefined
  },
  edit: (_, value) => {
    editedValue.value = value
  },
  end: () => {
    dropdownVisible.value = false
  },
})

function toggleDropdownWidget() {
  if (!dropdownVisible.value) dropDownInteraction.start()
  else dropDownInteraction.cancel()
}

function onClick(indexOfFiltered: number, keepOpen: boolean) {
  const clicked = filteredTags.value[indexOfFiltered]
  if (clicked?.tag instanceof CustomTag) clicked.tag.onClick()
  else selectedIndex.value = clicked?.index
  if (!keepOpen) {
    // We cancel interaction instead of ending it to restore the old value in the inner widget;
    // if we clicked already selected entry, there would be no AST change, thus the inner
    // widget's content woud not be updated.
    dropDownInteraction.cancel()
  }
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  let edit: Ast.MutableModule | undefined
  if (selectedTag.value instanceof CustomTag) {
    console.warn('Selecting custom drop down item does nothing!')
    return
  }
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
    [CustomDropdownItemsKey]?: readonly CustomDropdownItem[]
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
      v-if="dropdownVisible"
      ref="dropdownElement"
      class="dropdownContainer"
      :color="'var(--node-color-primary)'"
      :values="filteredTagLabels"
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
