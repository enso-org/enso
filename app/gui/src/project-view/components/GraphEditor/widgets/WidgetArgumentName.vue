<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { isRequiredArgument } from '$/providers/openedProjects/suggestionDatabase/entry'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import RequiredArgumentArrow from '@/components/GraphEditor/widgets/WidgetArgumentName/RequiredArgumentArrow.vue'
import WidgetMultiSelection from '@/components/GraphEditor/widgets/WidgetMultiSelection.vue'
import WidgetSelection from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { injectWidgetUsageInfo, usageKeyForInput } from '@/providers/widgetUsageInfo'
import { Ast } from '@/util/ast'
import { ApplicationKind, ArgumentInfoKey } from '@/util/callTree'
import { computed, useTemplateRef } from 'vue'
import type { SuggestionEntryArgument } from 'ydoc-shared/languageServerTypes/suggestions'

const props = defineProps(widgetProps(widgetDefinition))

const currentProject = useCurrentProject()
const graph = computed(() => currentProject.graph.value)
const portInfo = injectPortInfo(true)
const parentUsageInfo = injectWidgetUsageInfo(true)
const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputParentWidgets = computed(() =>
  parentUsageInfo?.usageKey === usageKey.value ? parentUsageInfo?.previouslyUsed : undefined,
)

const showArgumentValue = computed(() => {
  return (
    portInfo == null ||
    !portInfo.hasPersistedConnection ||
    (WidgetInput.isAst(props.input) && portInfo.portId !== props.input.value?.id)
  )
})

const missing = computed(
  () =>
    WidgetInput.isPlaceholder(props.input) && isRequiredArgument(props.input[ArgumentInfoKey].info),
)
const primary = computed(() => props.nesting < 2)

const innerInput = computed(() => ({
  ...props.input,
  [ArgumentNameShownKey]: true,
}))

const childWidgetRef = useTemplateRef<typeof NodeWidget>('childWidgetRef')
const isChildWidgetEmpty = computed(() => !childWidgetRef.value?.isSelected)
const visuallyHideArrow = computed(() => portInfo?.isVisualTarget ?? false)
const showArrow = computed(() => {
  const selectionWidgetsShown =
    sameInputParentWidgets.value?.has(WidgetSelection) ||
    sameInputParentWidgets.value?.has(WidgetMultiSelection)
  const otherWidgetsCanBeShown = showArgumentValue.value && !isChildWidgetEmpty.value
  return missing.value && !selectionWidgetsShown && !otherWidgetsCanBeShown
})
</script>

<script lang="ts">
function hasKnownArgumentName(input: WidgetInput): input is WidgetInput & {
  value: Exclude<WidgetInput['value'], Ast.Token>
  [ArgumentInfoKey]: { info: SuggestionEntryArgument }
} {
  return !WidgetInput.isToken(input) && input[ArgumentInfoKey]?.info != null
}

export const widgetDefinition = defineWidget(
  hasKnownArgumentName,
  {
    priority: 100,
    score: (props) => {
      const isTopArg =
        props.nesting < 2 && props.input[ArgumentInfoKey].appKind === ApplicationKind.Prefix
      return WidgetInput.isPlaceholder(props.input) || isTopArg ? Score.Perfect : Score.Mismatch
    },
  },
  import.meta.hot,
)

export const ArgumentNameShownKey: unique symbol = Symbol.for('WidgetInput:ArgumentNameShown')
</script>

<template>
  <div class="WidgetArgumentName widgetParent" :class="{ primary, missing }">
    <RequiredArgumentArrow
      v-if="showArrow"
      :hide="visuallyHideArrow"
      @arrowClick="graph.createEdgeFromPort(props.input.portId, $event)"
    />
    <span class="name widgetSingleLine">
      <span class="widgetApplyPadding" :class="{ widgetRounded: missing }">{{
        props.input[ArgumentInfoKey].info.name
      }}</span>
    </span>
    <NodeWidget v-if="showArgumentValue" ref="childWidgetRef" :input="innerInput" allowEmpty />
  </div>
</template>

<style scoped>
.WidgetArgumentName {
  gap: var(--widget-token-pad-unit);
}

.name {
  opacity: 0.6;
  border-radius: var(--node-port-border-radius);
  transition:
    background-color,
    color,
    opacity 0.2s ease;
  .missing & {
    opacity: 1;
    background-color: var(--color-missing-value);
    color: var(--color-node-text-missing-value);
  }
}
</style>
