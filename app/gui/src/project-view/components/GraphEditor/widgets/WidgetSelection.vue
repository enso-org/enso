<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type SuggestionEntryArgument } from '$/providers/openedProjects/suggestionDatabase/entry'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { singleChoiceConfiguration } from '$/providers/openedProjects/widgetRegistry/configuration'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SelectionArrow from '@/components/GraphEditor/widgets/WidgetSelection/SelectionArrow.vue'
import SelectionSubmenu from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import { activityDropdownStyles } from '@/components/GraphEditor/widgets/WidgetSelection/styles'
import {
  ActionTag,
  ExpressionTag,
  NestedChoiceTag,
  useExpressionTags,
  useTagEntries,
  type Actions,
  type CustomDropdownItem,
  type Entry,
} from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import OptionallyKeepAlive from '@/components/OptionallyKeepAlive.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import { unrefElement } from '@/composables/events'
import { usePopoverRoot } from '@/providers/popoverRoot'
import { provideSelectionArrow } from '@/providers/selectionArrow'
import { useTopLevelArgument } from '@/providers/topLevelArgument'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { ArgumentInfoKey } from '@/util/callTree'
import { arrayEquals } from '@/util/data/array'
import type { ToValue } from '@/util/reactivity'
import { computed, ref, shallowRef, toRef, toValue, useTemplateRef, type VNode } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const { module, projectNames: projectNames, suggestionDb: suggestionDbStore } = useCurrentProject()

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

const topLevelArgument = useTopLevelArgument(true)
const floatReference = computed(
  () =>
    topLevelArgument?.enclosingTopLevelArgument(widgetRoot.value, tree.rootElement) ??
    widgetRoot.value,
)

const { floatingStyles: activityStyles } = activityDropdownStyles(
  floatReference,
  activityElement,
  popoverRoot,
)

type ExpressionFilter = (tag: ExpressionTag) => boolean
function makeExpressionFilter(pattern: Ast.Ast | string | undefined): ExpressionFilter | undefined {
  if (!pattern) return undefined
  const editedAst = typeof pattern === 'string' ? Ast.parseExpression(pattern) : pattern
  if (editedAst instanceof Ast.TextLiteral) {
    const text = editedAst.rawTextContent
    if (!text) return undefined
    return (tag: ExpressionTag) =>
      (tag.expressionAst instanceof Ast.TextLiteral &&
        tag.expressionAst.rawTextContent.startsWith(text)) ||
      (tag.explicitLabel != null && tag.explicitLabel.startsWith(text))
  }
  const editedCode = pattern instanceof Ast.Ast ? pattern.code() : pattern
  if (editedCode) {
    return (tag: ExpressionTag) =>
      tag.expression.startsWith(editedCode) ||
      (tag.explicitLabel != null && tag.explicitLabel.startsWith(editedCode))
  }
  return undefined
}

const expressionTags = useExpressionTags({
  dynamicConfig: () => props.input.dynamicConfig,
  staticTags: () => props.input[ArgumentInfoKey]?.info?.tagValues,
  suggestionDb: () => suggestionDbStore.value.entries,
  projectNames,
})

const customTags = computed(
  () =>
    props.input[CustomDropdownItemsKey]?.map((entry) =>
      entry instanceof ExpressionTag ? entry : ActionTag.FromItem(entry),
    ) ?? [],
)

const filteredTags = computed(() => {
  const expressionFilter = makeExpressionFilter(editedValue.value)
  if (expressionFilter) {
    const flattened = expressionTags.value.flatMap((tag) =>
      tag instanceof NestedChoiceTag ? tag.flatten() : [tag],
    )
    const filteredCustomTags = customTags.value.filter(
      (tag) => tag instanceof ExpressionTag && expressionFilter(tag),
    )
    return [...filteredCustomTags, ...flattened.filter(expressionFilter)]
  } else {
    return [...customTags.value, ...expressionTags.value]
  }
})

const removeSurroundingParens = (expr?: string) => expr?.trim().replaceAll(/(^[(])|([)]$)/g, '')

const entries = useTagEntries(filteredTags, (expression) =>
  selectedExpressions.value.has(expression),
)

const selectedExpressions = computed(() => {
  const selected = new Set<string>()
  const code = removeSurroundingParens(WidgetInput.valueRepr(props.input))
  if (code?.includes(' ')) selected.add(code.substring(0, code.indexOf(' ')))
  if (code) selected.add(code)
  return selected
})
const innerWidgetInput = computed<WidgetInput>(() => {
  const dynamicConfig =
    props.input.dynamicConfig?.kind === 'Single_Choice' ?
      singleChoiceConfiguration(props.input.dynamicConfig)
    : props.input.dynamicConfig
  return {
    ...props.input,
    editHandler: dropDownInteraction.value,
    dynamicConfig,
  }
})

const selectionArrow = provideSelectionArrow({
  node: () => props.input.value,
  show: toRef(tree, 'extended'),
  isHovered,
})

function onClose() {
  activity.value = undefined
}

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
        props.updateCallback({
          portUpdate: { origin: props.input.portId, value: editedValue.value },
          directInteraction: false,
        })
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
  childEnded: () => {
    dropDownInteraction.value.end()
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
  else expressionTagClicked(clickedEntry.tag)
  if (!(keepOpen || activity.value)) {
    // We cancel interaction instead of ending it to restore the old value in the inner widget;
    // if we clicked already selected entry, there would be no AST change, thus the inner
    // widget's content would not be updated.
    dropDownInteraction.value.cancel()
  }
}

function expressionTagClicked(tag: ExpressionTag) {
  module.value.edit((edit) => {
    const tagValue = tag.resolveExpression(edit, module.value)
    return props.updateCallback({
      edit,
      portUpdate: { value: tagValue, origin: props.input.portId },
      directInteraction: true,
    })
  })
}
</script>

<script lang="ts">
/** An entry that can be added to a dropdown list by other parent widgets. */
export type DropdownItem = CustomDropdownItem | ExpressionTag
const CustomDropdownItemsKey: unique symbol = Symbol.for('WidgetInput:CustomDropdownItems')

/** Add extra dropdown items to a widget input. */
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
      // We don’t want to show the dropdown until the dynamic config is received
      // to avoid showing stale dropdown items. Custom dropdown items should be displayed without delay, though.
      props.input[CustomDropdownItemsKey] != null ? Score.Perfect
      : props.input.dynamicConfig?.kind === 'Pending' ? Score.Mismatch
      : props.input.dynamicConfig?.kind === 'Single_Choice' ? Score.Perfect
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
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [CustomDropdownItemsKey]?: readonly DropdownItem[]
  }
}
</script>

<template>
  <div
    ref="widgetRoot"
    class="WidgetSelection widgetParent clickable"
    @pointerdown.prevent
    @click.stop="toggleDropdownWidget"
    @keydown.enter.stop
    @pointerover="isHovered = true"
    @pointerout="isHovered = false"
  >
    <NodeWidget :input="innerWidgetInput" />
    <SelectionArrow v-if="selectionArrow" v-bind="selectionArrow" />
    <SelectionSubmenu
      ref="submenuRef"
      :floatReference="floatReference"
      :show="dropDownInteraction.isActive() && activity == null && entries.length > 0"
      :entries="entries"
      :topLevel="true"
      @clickedEntry="onClick"
    />

    <OptionallyKeepAlive :when="keepActivityAlive">
      <Teleport v-if="dropDownInteraction.isActive() && activity" :to="popoverRoot">
        <div
          ref="activityElement"
          class="activityElement widgetOutOfLayout"
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
  position: relative;
}

.activityElement {
  z-index: calc(var(--z-index-component-menu) + 1);
  --z-index-file-browser: calc(var(--z-index-component-menu) + 1);
}
</style>
