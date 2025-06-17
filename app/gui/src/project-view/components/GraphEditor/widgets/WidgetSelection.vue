<script setup lang="ts">
import {
  useGraphStore,
  useProjectNames,
  useSuggestionDbStore,
} from '$/components/WithCurrentProject.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { enclosingTopLevelArgument } from '@/components/GraphEditor/widgets/WidgetTopLevelArgument.vue'
import OptionallyKeepAlive from '@/components/OptionallyKeepAlive.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { unrefElement } from '@/composables/events'
import { usePopoverRoot } from '@/providers/popoverRoot'
import { injectSelectionArrow, provideSelectionArrow } from '@/providers/selectionArrow'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  Choice,
  multipleChoiceConfiguration,
  singleChoiceConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { injectWidgetTree } from '@/providers/widgetTree'
import { type SuggestionEntryArgument } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { ArgumentInfoKey } from '@/util/callTree'
import { arrayEquals } from '@/util/data/array'
import { type ToValue } from '@/util/reactivity'
import type { RendererNode, VNode } from 'vue'
import { computed, proxyRefs, ref, shallowRef, toValue, useTemplateRef, watch } from 'vue'
import SelectionSubmenu from './WidgetSelection/SelectionSubmenu.vue'
import { activityDropdownStyles } from './WidgetSelection/styles'
import {
  ActionTag,
  Actions,
  CustomDropdownItem,
  Entry,
  ExpressionTag,
  NestedChoiceTag,
} from './WidgetSelection/tags'

const props = defineProps(widgetProps(widgetDefinition))
const suggestions = useSuggestionDbStore()
const graph = useGraphStore()
const projectNames = useProjectNames()

const tree = injectWidgetTree()

const widgetRoot = useTemplateRef<HTMLElement>('widgetRoot')
const submenuRef = useTemplateRef('submenuRef')
const activityElement = useTemplateRef<HTMLElement>('activityElement')
const popoverRoot = usePopoverRoot()

const editedWidget = ref<string>()
const editedValue = ref<Ast.Owned<Ast.MutableExpression> | string | undefined>()
const isHovered = ref(false)
/** See {@link Actions.setActivity} */
const activity = shallowRef<ToValue<VNode>>()
const keepActivityAlive = ref(false)

const floatReference = computed(
  () => enclosingTopLevelArgument(widgetRoot.value, tree.rootElement) ?? widgetRoot.value,
)

const { floatingStyles: activityStyles } = activityDropdownStyles(
  floatReference,
  activityElement,
  popoverRoot,
)

type ExpressionFilter = (tag: ExpressionTag) => boolean
function makeExpressionFilter(pattern: Ast.Ast | string): ExpressionFilter | undefined {
  const editedAst = typeof pattern === 'string' ? Ast.parseExpression(pattern) : pattern
  if (editedAst instanceof Ast.TextLiteral) {
    return (tag: ExpressionTag) =>
      (tag.expressionAst instanceof Ast.TextLiteral &&
        tag.expressionAst.rawTextContent.startsWith(editedAst.rawTextContent)) ||
      (tag.explicitLabel != null && tag.explicitLabel.startsWith(editedAst.rawTextContent))
  }
  const editedCode = pattern instanceof Ast.Ast ? pattern.code() : pattern
  if (editedCode) {
    return (tag: ExpressionTag) =>
      tag.expression.startsWith(editedCode) ||
      (tag.explicitLabel != null && tag.explicitLabel.startsWith(editedCode))
  }
  return undefined
}

const staticTags = computed<ExpressionTag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  const suggestionDb = suggestions.entries
  return tags.map((t) => ExpressionTag.FromExpression(suggestionDb, projectNames, t))
})

const dynamicTags = computed<(ExpressionTag | NestedChoiceTag)[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice' && config?.kind !== 'Multiple_Choice') return []

  const choiceToTag = (choice: Choice): ExpressionTag | NestedChoiceTag => {
    if (choice.value instanceof Array) {
      return new NestedChoiceTag(choice.label ?? '…', choice.value.map(choiceToTag))
    } else {
      return ExpressionTag.FromExpression(
        suggestions.entries,
        projectNames,
        choice.value,
        choice.label,
        choice.icon,
      )
    }
  }

  return config.values.map(choiceToTag)
})

const allowExtendingUpwards = computed(() => ArgumentInfoKey in props.input)

const filteredTags = computed(() => {
  const expressionTags = dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value
  const customTags =
    props.input[CustomDropdownItemsKey]?.map((entry) =>
      entry instanceof ExpressionTag ? entry : ActionTag.FromItem(entry),
    ) ?? []
  const expressionFilter =
    !isMulti.value && editedValue.value && makeExpressionFilter(editedValue.value)
  if (expressionFilter) {
    const flattened = expressionTags.flatMap((tag) =>
      tag instanceof NestedChoiceTag ? tag.flatten() : [tag],
    )
    const filteredCustomTags = customTags.filter(
      (tag) => tag instanceof ExpressionTag && expressionFilter(tag),
    )
    return [...filteredCustomTags, ...flattened.filter(expressionFilter)]
  } else {
    return [...customTags, ...expressionTags]
  }
})

const entries = computed<Entry[]>(() => filteredTags.value.map(tagToEntry))

function tagToEntry(tag: ExpressionTag | NestedChoiceTag | ActionTag): Entry {
  return {
    value: tag.label,
    key: tag instanceof ExpressionTag ? tag.toString() : undefined,
    selected: tag instanceof ExpressionTag && selectedExpressions.value.has(tag.expression),
    icon: tag instanceof ExpressionTag || tag instanceof ActionTag ? tag.icon : undefined,
    tag,
    isNested: tag instanceof NestedChoiceTag,
    nestedValues: tag instanceof NestedChoiceTag ? tag.choices.map(tagToEntry) : [],
  }
}

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
    editHandler: dropDownInteraction.value,
    dynamicConfig,
  }
})

function selectionArrowTarget(ast: Ast.Expression): Ast.Expression | Ast.Token | null {
  let node = ast
  // If the input is a constructor application, place the arrow under the constructor name.
  while (node instanceof Ast.Ast) {
    if (node instanceof Ast.AutoscopedIdentifier) return node.identifier
    else if (node instanceof Ast.PropertyAccess) return node.rhs
    else if (node instanceof Ast.App) node = node.function
    else if (node instanceof Ast.Group && node.expression) node = node.expression
    else break
  }
  return null
}

const parentSelectionArrow = injectSelectionArrow(true)
const arrowSuppressed = ref(false)
const showArrow = computed(() => !arrowSuppressed.value && (tree.extended || isHovered.value))
provideSelectionArrow(
  proxyRefs({
    id: computed((): Ast.AstId | Ast.TokenId | null => {
      const node = props.input.value
      if (!(node instanceof Ast.Ast)) return null
      if (!node.isExpression()) return null
      const target = selectionArrowTarget(node)
      return target ? target.id : null
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
const dropDownInteraction = WidgetEditHandler.New(props, {
  cancel: onClose,
  end: onClose,
  pointerdown: (e) => {
    if (
      submenuRef.value?.isTargetOutside(e) &&
      (activityElement.value == null || targetIsOutside(e, unrefElement(activityElement))) &&
      targetIsOutside(e, unrefElement(widgetRoot)) &&
      targetIsOutside(e, document.getElementById('floatingLayer'))
    ) {
      dropDownInteraction.value.end()
      if (editedWidget.value)
        props.onUpdate({
          portUpdate: { origin: props.input.portId, value: editedValue.value },
          directInteraction: false,
        })
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
    dropDownInteraction.value.start()
    return true
  },
  childEnded: () => {
    if (!isMulti.value) dropDownInteraction.value.end()
  },
})

function toggleDropdownWidget() {
  if (!dropDownInteraction.value.isActive()) dropDownInteraction.value.start()
  else dropDownInteraction.value.cancel()
}

const dropdownActions: Actions = {
  setActivity: (newActivity: ToValue<VNode>, keepAlive = false) => {
    activity.value = newActivity
    keepActivityAlive.value = keepAlive
  },
  close: () => dropDownInteraction.value.end(),
}

function onClick(clickedEntry: Entry, keepOpen: boolean) {
  if (clickedEntry.tag instanceof ActionTag) clickedEntry.tag.onClick(dropdownActions)
  else if (clickedEntry.tag instanceof NestedChoiceTag) return
  else expressionTagClicked(clickedEntry.tag, clickedEntry.selected)
  if (!(keepOpen || isMulti.value || activity.value)) {
    // We cancel interaction instead of ending it to restore the old value in the inner widget;
    // if we clicked already selected entry, there would be no AST change, thus the inner
    // widget's content would not be updated.
    dropDownInteraction.value.cancel()
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
    vector.push(Ast.parseExpression(value, vector.module)!)
  }
}

function expressionTagClicked(tag: ExpressionTag, previousState: boolean) {
  const edit = graph.startEdit()
  const directInteraction = true
  const tagValue = resolveTagExpression(edit, tag)
  if (isMulti.value) {
    const inputValue = editedValue.value ?? props.input.value
    if (inputValue instanceof Ast.Vector) {
      toggleVectorValue(edit.getVersion(inputValue), tagValue, previousState)
      props.onUpdate({ edit, directInteraction })
    } else {
      const vector = Ast.Vector.new(
        edit,
        inputValue instanceof Ast.Ast ? [edit.take(inputValue.id)] : [],
      )
      toggleVectorValue(vector, tagValue, previousState)
      props.onUpdate({
        edit,
        portUpdate: { value: vector, origin: props.input.portId },
        directInteraction,
      })
    }
  } else {
    props.onUpdate({
      edit,
      portUpdate: { value: tagValue, origin: props.input.portId },
      directInteraction,
    })
  }
}

const arrowLocation = ref()
</script>

<script lang="ts">
/** An entry that can be added to a dropdown list by other parent widgets. */
export type DropdownItem = CustomDropdownItem | ExpressionTag
const CustomDropdownItemsKey: unique symbol = Symbol.for('WidgetInput:CustomDropdownItems')

/** Add extra dropdown items to a widget input. */
// eslint-disable-next-line jsdoc/require-jsdoc
export function withDropdownItems(input: WidgetInput, items: Iterable<DropdownItem>): WidgetInput {
  const existingItems = input[CustomDropdownItemsKey] ?? []
  return { ...input, [CustomDropdownItemsKey]: [...existingItems, ...items] }
}

function isHandledByCheckboxWidget(parameter: SuggestionEntryArgument | undefined): boolean {
  return (
    parameter?.tagValues != null &&
    arrayEquals(Array.from(parameter.tagValues).sort(), ['False', 'True'])
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

export { CustomDropdownItemsKey }
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [CustomDropdownItemsKey]?: readonly DropdownItem[]
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
    <teleport v-if="showArrow" :disabled="!arrowLocation" :to="arrowLocation">
      <SvgIcon
        name="arrow_right_head_only"
        class="arrow widgetOutOfLayout"
        :class="{ hovered: isHovered }"
      />
    </teleport>
    <SelectionSubmenu
      ref="submenuRef"
      :floatReference="floatReference"
      :show="dropDownInteraction.isActive() && activity == null"
      :entries="entries"
      :topLevel="true"
      :extendUpwards="allowExtendingUpwards"
      @clickedEntry="onClick"
    />

    <OptionallyKeepAlive :when="keepActivityAlive">
      <Teleport v-if="dropDownInteraction.isActive() && activity" :to="popoverRoot">
        <div
          ref="activityElement"
          class="activityElement widgetOutOfLayout floatingElement"
          :style="activityStyles"
        >
          <SizeTransition height :duration="100">
            <component :is="toValue(activity)" />
          </SizeTransition>
        </div>
      </Teleport>
    </OptionallyKeepAlive>
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
  transition: opacity 0.2s ease-in-out;
  &.hovered {
    opacity: 0.9;
  }
}

.activityElement {
  /* Above the circular menu. */
  z-index: 26;
}
</style>
