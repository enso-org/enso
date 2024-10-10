<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { enclosingTopLevelArgument } from '@/components/GraphEditor/widgets/WidgetTopLevelArgument.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import DropdownWidget, { type DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { injectSelectionArrow, provideSelectionArrow } from '@/providers/selectionArrow'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  multipleChoiceConfiguration,
  singleChoiceConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { requiredImports, type RequiredImport } from '@/stores/graph/imports'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import {
  type SuggestionEntry,
  type SuggestionEntryArgument,
} from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { ArgumentInfoKey } from '@/util/callTree'
import { arrayEquals } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import { autoUpdate, offset, shift, size, useFloating } from '@floating-ui/vue'
import type { Ref, RendererNode, VNode } from 'vue'
import { computed, proxyRefs, ref, shallowRef, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()

const tree = injectWidgetTree()

const widgetRoot = shallowRef<HTMLElement>()
const dropdownElement = ref<HTMLElement>()
const activityElement = ref<HTMLElement>()

const editedWidget = ref<string>()
const editedValue = ref<Ast.Owned | string | undefined>()
const isHovered = ref(false)
/** See @{link Actions.setActivity} */
const activity = shallowRef<VNode>()

// How much wider a dropdown can be than a port it is attached to, when a long text is present.
// Any text beyond that limit will receive an ellipsis and sliding animation on hover.
const MAX_DROPDOWN_OVERSIZE_PX = 150

const floatReference = computed(
  () => enclosingTopLevelArgument(widgetRoot.value, tree) ?? widgetRoot.value,
)

function dropdownStyles(dropdownElement: Ref<HTMLElement | undefined>, limitWidth: boolean) {
  return useFloating(floatReference, dropdownElement, {
    placement: 'bottom-start',
    middleware: computed(() => {
      return [
        offset((state) => {
          const NODE_HEIGHT = 32
          return {
            mainAxis: (NODE_HEIGHT - state.rects.reference.height) / 2,
          }
        }),
        size(() => ({
          elementContext: 'reference',
          apply({ elements, rects, availableWidth }) {
            const PORT_PADDING_X = 8
            const screenOverflow = Math.max(
              (rects.floating.width - availableWidth) / 2 + PORT_PADDING_X,
              0,
            )
            const portWidth = rects.reference.width + PORT_PADDING_X * 2

            const minWidth = `${Math.max(portWidth - screenOverflow, 0)}px`
            const maxWidth = limitWidth ? `${portWidth + MAX_DROPDOWN_OVERSIZE_PX}px` : null

            Object.assign(elements.floating.style, { minWidth, maxWidth })
            elements.floating.style.setProperty('--dropdown-max-width', maxWidth)
          },
        })),
        // Try to keep the dropdown within node's bounds.
        shift(() => (tree.nodeElement ? { boundary: tree.nodeElement } : {})),
        shift(), // Always keep within screen bounds, overriding node bounds.
      ]
    }),
    whileElementsMounted: autoUpdate,
  })
}

const { floatingStyles } = dropdownStyles(dropdownElement, true)
const { floatingStyles: activityStyles } = dropdownStyles(activityElement, false)

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
    readonly onClick: (dropdownActions: Actions) => void,
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
    for (const element of getValues(editedValue.value ?? props.input.value)) {
      const normalized = removeSurroundingParens(element.code())
      if (normalized) selected.add(normalized)
    }
  } else {
    const code = removeSurroundingParens(WidgetInput.valueRepr(props.input))
    if (code?.includes(' ')) selected.add(code.substring(0, code.indexOf(' ')))
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

const parentSelectionArrow = injectSelectionArrow(true)
const arrowSuppressed = ref(false)
const showArrow = computed(() => isHovered.value && !arrowSuppressed.value)
provideSelectionArrow(
  proxyRefs({
    id: computed(() => {
      // Find the top-most PropertyAccess, and return its rhs id.
      // It will be used to place the dropdown arrow under the constructor name.
      let node = props.input.value
      while (node instanceof Ast.Ast) {
        if (node instanceof Ast.AutoscopedIdentifier) return node.identifier.id
        if (node instanceof Ast.PropertyAccess) return node.rhs.id
        if (node instanceof Ast.App) node = node.function
        else {
          const wrapped = node.wrappedExpression()
          if (wrapped != null) node = wrapped
          else break
        }
      }
      return null
    }),
    requestArrow: (target: RendererNode) => {
      arrowLocation.value = target
    },
    handled: false,
    get suppressArrow() {
      return arrowSuppressed.value
    },
    set suppressArrow(value) {
      arrowSuppressed.value = value
    },
  }),
)

watch(showArrow, (arrowShown) => {
  if (parentSelectionArrow) {
    parentSelectionArrow.suppressArrow = arrowShown
  }
})

function onClose() {
  activity.value = undefined
}

const isMulti = computed(() => props.input.dynamicConfig?.kind === 'Multiple_Choice')
const dropDownInteraction = WidgetEditHandler.New('WidgetSelection', props.input, {
  cancel: onClose,
  end: onClose,
  pointerdown: (e) => {
    if (
      targetIsOutside(e, unrefElement(dropdownElement)) &&
      targetIsOutside(e, unrefElement(activityElement)) &&
      targetIsOutside(e, unrefElement(widgetRoot))
    ) {
      dropDownInteraction.end()
      if (editedWidget.value)
        props.onUpdate({ portUpdate: { origin: props.input.portId, value: editedValue.value } })
    } else if (isMulti.value) {
      // In multi-select mode the children contain actual values; when a dropdown click occurs,
      // we allow the event to propagate so the child widget can commit before the dropdown-toggle occurs.
      // We don't do this in single-select mode because the value is treated as a filter in that case,
      // so it shouldn't be committed as a value before the dropdown operation.
      return false
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
  childEnded: () => {
    if (!isMulti.value) dropDownInteraction.end()
  },
})

function toggleDropdownWidget() {
  if (!dropDownInteraction.isActive()) dropDownInteraction.start()
  else dropDownInteraction.cancel()
}

const dropdownActions: Actions = {
  setActivity: (newActivity) => {
    activity.value = newActivity
  },
  close: dropDownInteraction.end.bind(dropDownInteraction),
}

function onClick(clickedEntry: Entry, keepOpen: boolean) {
  if (clickedEntry.tag instanceof ActionTag) clickedEntry.tag.onClick(dropdownActions)
  else expressionTagClicked(clickedEntry.tag, clickedEntry.selected)
  if (!(keepOpen || isMulti.value || activity.value)) {
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
    const inputValue = editedValue.value ?? props.input.value
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
const CustomDropdownItemsKey: unique symbol = Symbol.for('WidgetInput:CustomDropdownItems')

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
  onClick: (dropdownActions: Actions) => void
}

/** Actions a {@link CustomDropdownItem} may perform when clicked. */
export interface Actions {
  /**
   * Provide an alternative dialog to be rendered in place of the dropdown.
   *
   * For example, the {@link WidgetCloudBrowser} installs a custom entry that, when clicked,
   * opens a file browser where the dropdown was.
   */
  setActivity: (activity: VNode) => void
  close: () => void
}

export { CustomDropdownItemsKey }
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [CustomDropdownItemsKey]?: readonly CustomDropdownItem[]
  }
}
</script>

<template>
  <div
    ref="widgetRoot"
    class="WidgetSelection clickable"
    :class="{ multiSelect: isMulti }"
    @click.stop="toggleDropdownWidget"
    @pointerover="isHovered = true"
    @pointerout="isHovered = false"
  >
    <NodeWidget :input="innerWidgetInput" />
    <Teleport v-if="showArrow" defer :disabled="!arrowLocation" :to="arrowLocation">
      <SvgIcon name="arrow_right_head_only" class="arrow widgetOutOfLayout" />
    </Teleport>
    <Teleport v-if="tree.nodeElement" :to="tree.nodeElement">
      <div ref="dropdownElement" :style="floatingStyles" class="widgetOutOfLayout floatingElement">
        <SizeTransition height :duration="100">
          <DropdownWidget
            v-if="dropDownInteraction.isActive() && activity == null"
            color="var(--node-color-primary)"
            :entries="entries"
            @clickEntry="onClick"
          />
        </SizeTransition>
      </div>
      <div
        ref="activityElement"
        class="activityElement widgetOutOfLayout floatingElement"
        :style="activityStyles"
      >
        <SizeTransition height :duration="100">
          <div v-if="dropDownInteraction.isActive() && activity">
            <component :is="activity" />
          </div>
        </SizeTransition>
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
.WidgetSelection {
  display: flex;
  flex-direction: row;
  align-items: center;
  position: relative;
  min-height: var(--node-port-height);
}

.floatingElement {
  z-index: 21;
}

svg.arrow {
  position: absolute;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
}

.activityElement {
  --background-color: var(--node-color-primary);
  /* Above the circular menu. */
  z-index: 26;
}
</style>
