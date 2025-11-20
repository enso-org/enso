<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { multipleChoiceConfiguration } from '$/providers/openedProjects/widgetRegistry/configuration'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SelectionArrow from '@/components/GraphEditor/widgets/WidgetSelection/SelectionArrow.vue'
import SelectionSubmenu from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import {
  ExpressionTag,
  NestedChoiceTag,
  useExpressionTags,
  useTagEntries,
} from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { unrefElement } from '@/composables/events'
import { provideSelectionArrow } from '@/providers/selectionArrow'
import { useTopLevelArgument } from '@/providers/topLevelArgument'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed, ref, toRef, useTemplateRef } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const { module, projectNames: projectNames, suggestionDb: suggestionDbStore } = useCurrentProject()

const tree = injectWidgetTree()

const widgetRoot = useTemplateRef<HTMLElement>('widgetRoot')
const submenuRef = useTemplateRef('submenuRef')

const editedWidget = ref<string>()
const editedValue = ref<Ast.Owned<Ast.MutableExpression> | string | undefined>()
const isHovered = ref(false)

const topLevelArgument = useTopLevelArgument(true)
const floatReference = computed(
  () =>
    topLevelArgument?.enclosingTopLevelArgument(widgetRoot.value, tree.rootElement) ??
    widgetRoot.value,
)

const expressionTags = useExpressionTags({
  dynamicConfig: () => props.input.dynamicConfig,
  staticTags: () => props.input[ArgumentInfoKey]?.info?.tagValues,
  suggestionDb: () => suggestionDbStore.value?.entries,
  projectNames,
})

const entries = useTagEntries(expressionTags, (expression) =>
  selectedExpressions.value.has(expression),
)

const selectedExpressions = computed(() => {
  const selected = new Set<string>()
  for (const element of getValues(editedValue.value ?? props.input.value)) {
    const normalized = Ast.unwrapGroups(element).code().trim()
    if (normalized) selected.add(normalized)
  }
  return selected
})
const innerWidgetInput = computed<WidgetInput>(() => {
  const dynamicConfig =
    props.input.dynamicConfig?.kind === 'Multiple_Choice' ?
      multipleChoiceConfiguration(props.input.dynamicConfig)
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

const dropDownInteraction = WidgetEditHandler.New(props, {
  pointerdown: (e) => {
    if (
      submenuRef.value?.isTargetOutside(e) &&
      targetIsOutside(e, unrefElement(widgetRoot)) &&
      targetIsOutside(e, document.getElementById('floatingLayer'))
    ) {
      dropDownInteraction.value.end()
    }
    // Allow the event to propagate so the child widget can commit.
    return false
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
})

function toggleDropdownWidget() {
  if (!dropDownInteraction.value.isActive()) dropDownInteraction.value.start()
  else dropDownInteraction.value.cancel()
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

function onClick({
  tag,
  selected: previousState,
}: {
  tag: ExpressionTag | NestedChoiceTag
  selected: boolean
}) {
  if (tag instanceof NestedChoiceTag) return

  module.value.edit((edit) => {
    const directInteraction = true
    const tagValue = tag.resolveExpression(edit, module.value)
    const inputValue = editedValue.value ?? props.input.value
    if (inputValue instanceof Ast.Vector) {
      toggleVectorValue(edit.getVersion(inputValue), tagValue, previousState)
      return props.updateCallback({ edit, directInteraction })
    } else {
      const vector = Ast.Vector.new(
        edit,
        inputValue instanceof Ast.Ast ? [edit.take(inputValue.id)] : [],
      )
      toggleVectorValue(vector, tagValue, previousState)
      return props.updateCallback({
        edit,
        portUpdate: { value: vector, origin: props.input.portId },
        directInteraction,
      })
    }
  })
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 50,
    score: (props) =>
      props.input.dynamicConfig?.kind === 'Multiple_Choice' ? Score.Perfect : Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <div
    ref="widgetRoot"
    class="WidgetMultiSelection widgetParent clickable"
    @click.stop="toggleDropdownWidget"
    @pointerover="isHovered = true"
    @pointerout="isHovered = false"
  >
    <NodeWidget :input="innerWidgetInput" />
    <SelectionArrow v-if="selectionArrow" v-bind="selectionArrow" />
    <SelectionSubmenu
      ref="submenuRef"
      :floatReference="floatReference"
      :show="dropDownInteraction.isActive()"
      :entries="entries"
      :topLevel="true"
      @clickedEntry="onClick"
    />
  </div>
</template>

<style scoped>
.WidgetMultiSelection {
  position: relative;
}
</style>
