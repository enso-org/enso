<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget, { type DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { provideSelectionArrow } from '@/providers/selectionArrow.ts'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import {
  multipleChoiceConfiguration,
  singleChoiceConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { injectWidgetTree } from '@/providers/widgetTree.ts'
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
import { autoUpdate, offset, size, useFloating } from '@floating-ui/vue'
import { computed, proxyRefs, ref, type ComponentInstance, type RendererNode } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()

const tree = injectWidgetTree()

const widgetRoot = ref<HTMLElement>()
const dropdownElement = ref<ComponentInstance<typeof DropdownWidget>>()

const editedWidget = ref<string>()
const editedValue = ref<Ast.Owned | string | undefined>()
const isHovered = ref(false)

const { floatingStyles } = useFloating(widgetRoot, dropdownElement, {
  middleware: computed(() => {
    return [
      offset((state) => {
        const NODE_HEIGHT = 32
        return {
          mainAxis: (NODE_HEIGHT - state.rects.reference.height) / 2,
        }
      }),
      size({
        apply({ elements, rects }) {
          Object.assign(elements.floating.style, {
            minWidth: `${rects.reference.width + 16}px`,
          })
        },
      }),
    ]
  }),
  whileElementsMounted: autoUpdate,
})

class ExpressionTag {
  private cachedExpressionAst: Ast.Ast | undefined

  constructor(
    readonly expression: string,
    private explicitLabel?: Opt<string>,
    readonly requiredImports?: RequiredImport[],
    public parameters?: ArgumentWidgetConfiguration[],
  ) {}

  static FromQualifiedName(qn: Ast.QualifiedName, label?: Opt<string>): ExpressionTag {
    const entry = suggestions.entries.getEntryByQualifiedName(qn)
    if (entry) return ExpressionTag.FromEntry(entry, label)
    return new ExpressionTag(qn, label ?? qnLastSegment(qn))
  }

  static FromExpression(expression: string, label?: Opt<string>): ExpressionTag {
    const qn = tryQualifiedName(expression)
    if (qn.ok) return ExpressionTag.FromQualifiedName(qn.value, label)
    return new ExpressionTag(expression, label)
  }

  static FromEntry(entry: SuggestionEntry, label?: Opt<string>): ExpressionTag {
    const expression =
      entry.selfType != null ? `_.${entry.name}`
      : entry.memberOf ? `${qnLastSegment(entry.memberOf)}.${entry.name}`
      : entry.name
    return new ExpressionTag(
      expression,
      label ?? entry.name,
      requiredImports(suggestions.entries, entry),
    )
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
}

class ActionTag {
  constructor(
    readonly label: string,
    readonly onClick: () => void,
  ) {}

  static FromItem(item: CustomDropdownItem): ActionTag {
    return new ActionTag(item.label, item.onClick)
  }
}

type ExpressionFilter = (tag: ExpressionTag) => boolean
function makeExpressionFilter(pattern: Ast.Ast | string): ExpressionFilter | undefined {
  const editedAst = typeof pattern === 'string' ? Ast.parse(pattern) : pattern
  const editedCode = pattern instanceof Ast.Ast ? pattern.code() : pattern
  if (editedAst instanceof Ast.TextLiteral) {
    return (tag: ExpressionTag) =>
      tag.expressionAst instanceof Ast.TextLiteral &&
      tag.expressionAst.rawTextContent.startsWith(editedAst.rawTextContent)
  }
  if (editedCode) {
    return (tag: ExpressionTag) => tag.expression.startsWith(editedCode)
  }
  return undefined
}

const staticTags = computed<ExpressionTag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  return tags.map((t) => ExpressionTag.FromExpression(t))
})

const dynamicTags = computed<ExpressionTag[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice' && config?.kind !== 'Multiple_Choice') return []

  return config.values.map((value) => {
    const tag = ExpressionTag.FromExpression(value.value, value.label)
    tag.parameters = value.parameters
    return tag
  })
})

const filteredTags = computed(() => {
  const expressionTags = dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value
  const expressionFilter =
    !isMulti.value && editedValue.value && makeExpressionFilter(editedValue.value)
  if (expressionFilter) {
    return expressionTags.filter(expressionFilter)
  } else {
    const actionTags = props.input[CustomDropdownItemsKey]?.map(ActionTag.FromItem) ?? []
    return [...actionTags, ...expressionTags]
  }
})
interface Entry extends DropdownEntry {
  tag: ExpressionTag | ActionTag
}
const entries = computed<Entry[]>(() => {
  return filteredTags.value.map((tag, _index) => ({
    value: tag.label,
    selected: tag instanceof ExpressionTag && selectedExpressions.value.has(tag.expression),
    tag,
  }))
})

const removeSurroundingParens = (expr?: string) => expr?.trim().replaceAll(/(^[(])|([)]$)/g, '')

const selectedExpressions = computed(() => {
  const selected = new Set<string>()
  if (isMulti.value) {
    for (const element of getValues(props.input.value)) {
      const normalized = removeSurroundingParens(element.code())
      if (normalized) selected.add(normalized)
    }
  } else {
    const code = removeSurroundingParens(WidgetInput.valueRepr(props.input))
    if (code) selected.add(code)
  }
  return selected
})
const innerWidgetInput = computed<WidgetInput>(() => {
  const dynamicConfig =
    props.input.dynamicConfig?.kind === 'Single_Choice' ?
      singleChoiceConfiguration(props.input.dynamicConfig)
    : props.input.dynamicConfig?.kind === 'Multiple_Choice' ?
      multipleChoiceConfiguration(props.input.dynamicConfig)
    : props.input.dynamicConfig
  return {
    ...props.input,
    editHandler: dropDownInteraction,
    dynamicConfig,
  }
})

provideSelectionArrow(
  proxyRefs({
    id: computed(() => {
      // Find the top-most PropertyAccess, and return its rhs id.
      // It will be used to place the dropdown arrow under the constructor name.
      let node = props.input.value
      while (node instanceof Ast.Ast) {
        if (node instanceof Ast.PropertyAccess) return node.rhs.id
        if (node instanceof Ast.App) node = node.function
        else break
      }
      return null
    }),
    requestArrow: (target: RendererNode) => {
      arrowLocation.value = target
    },
    handled: false,
  }),
)

const isMulti = computed(() => props.input.dynamicConfig?.kind === 'Multiple_Choice')
const dropDownInteraction = WidgetEditHandler.New('WidgetSelection', props.input, {
  cancel: () => {},
  pointerdown: (e, _) => {
    if (targetIsOutside(e, unrefElement(dropdownElement))) {
      dropDownInteraction.end()
      if (editedWidget.value)
        props.onUpdate({ portUpdate: { origin: props.input.portId, value: editedValue.value } })
    }
  },
  start: () => {
    editedWidget.value = undefined
    editedValue.value = undefined
  },
  edit: (origin, value) => {
    editedWidget.value = origin
    editedValue.value = value
  },
  addItem: () => {
    dropDownInteraction.start()
    return true
  },
})

function toggleDropdownWidget() {
  if (!dropDownInteraction.active.value) dropDownInteraction.start()
  else dropDownInteraction.cancel()
}

function onClick(clickedEntry: Entry, keepOpen: boolean) {
  if (clickedEntry.tag instanceof ActionTag) clickedEntry.tag.onClick()
  else expressionTagClicked(clickedEntry.tag, clickedEntry.selected)
  if (!(keepOpen || isMulti.value)) {
    // We cancel interaction instead of ending it to restore the old value in the inner widget;
    // if we clicked already selected entry, there would be no AST change, thus the inner
    // widget's content would not be updated.
    dropDownInteraction.cancel()
  }
}

/** Add any necessary imports for `tag`, and return it with any necessary qualification. */
function resolveTagExpression(edit: Ast.MutableModule, tag: ExpressionTag) {
  if (tag.requiredImports) {
    const conflicts = graph.addMissingImports(edit, tag.requiredImports)
    if (conflicts != null && conflicts.length > 0) {
      // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
      // https://github.com/enso-org/enso/issues/9356
      // And here it was wrong anyway: we should replace only conflicting name, not entire expression!
      // // Is there is a conflict, it would be a single one, because we only ask about a single entry.
      // return conflicts[0]?.fullyQualified!
    }
  }
  // Unless a conflict occurs, we use the selected expression as is.
  return tag.expression
}

function* getValues(expression: Ast.Ast | string | undefined) {
  if (expression instanceof Ast.Vector) {
    yield* expression.values()
  } else if (expression instanceof Ast.Ast) {
    yield expression
  }
}

function toggleVectorValue(vector: Ast.MutableVector, value: string, previousState: boolean) {
  if (previousState) {
    vector.keep((ast) => ast.code() !== value)
  } else {
    vector.push(Ast.parse(value, vector.module))
  }
}

function expressionTagClicked(tag: ExpressionTag, previousState: boolean) {
  const edit = graph.startEdit()
  const tagValue = resolveTagExpression(edit, tag)
  if (isMulti.value) {
    const inputValue = props.input.value
    if (inputValue instanceof Ast.Vector) {
      toggleVectorValue(edit.getVersion(inputValue), tagValue, previousState)
      props.onUpdate({ edit })
    } else {
      const vector = Ast.Vector.new(
        edit,
        inputValue instanceof Ast.Ast ? [edit.take(inputValue.id)] : [],
      )
      toggleVectorValue(vector, tagValue, previousState)
      props.onUpdate({ edit, portUpdate: { value: vector, origin: props.input.portId } })
    }
  } else {
    props.onUpdate({ edit, portUpdate: { value: tagValue, origin: props.input.portId } })
  }
}

const arrowLocation = ref()
</script>

<script lang="ts">
function isHandledByCheckboxWidget(parameter: SuggestionEntryArgument | undefined): boolean {
  return (
    parameter?.tagValues != null &&
    arrayEquals(Array.from(parameter.tagValues).sort(), [
      'Standard.Base.Data.Boolean.Boolean.False',
      'Standard.Base.Data.Boolean.Boolean.True',
    ])
  )
}

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 50,
    score: (props) =>
      props.input[CustomDropdownItemsKey] != null ? Score.Perfect
      : props.input.dynamicConfig?.kind === 'Single_Choice' ? Score.Perfect
      : props.input.dynamicConfig?.kind === 'Multiple_Choice' ? Score.Perfect
      : isHandledByCheckboxWidget(props.input[ArgumentInfoKey]?.info) ? Score.Mismatch
        // TODO[ao] here, instead of checking for existing dynamic config, we should rather return
        // Score.Good. But this does not work with WidgetArgument which would then take precedence
        // over selection (and we want to have name always under it)
      : props.input[ArgumentInfoKey]?.info?.tagValues != null && props.input.dynamicConfig == null ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)

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
    :class="{ multiSelect: isMulti }"
    @pointerdown.stop
    @pointerup.stop
    @click.stop="toggleDropdownWidget"
    @pointerover="isHovered = true"
    @pointerout="isHovered = false"
  >
    <NodeWidget :input="innerWidgetInput" />
    <Teleport v-if="arrowLocation" :to="arrowLocation">
      <SvgIcon v-if="isHovered" name="arrow_right_head_only" class="arrow" />
    </Teleport>
    <SvgIcon v-else-if="isHovered" name="arrow_right_head_only" class="arrow" />
    <Teleport v-if="tree.nodeElement" :to="tree.nodeElement">
      <SizeTransition height :duration="100">
        <DropdownWidget
          v-if="dropDownInteraction.active.value"
          ref="dropdownElement"
          :style="floatingStyles"
          :color="'var(--node-color-primary)'"
          :entries="entries"
          @clickEntry="onClick"
        />
      </SizeTransition>
    </Teleport>
  </div>
</template>

<style scoped>
.WidgetSelection {
  display: flex;
  flex-direction: row;
  align-items: center;
  position: relative;
  min-height: --node-port-height;
}

.arrow {
  position: absolute;
  pointer-events: none;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
}
</style>
