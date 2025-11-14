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
import { MultiSelectionWidgetShownKey } from '@/components/GraphEditor/widgets/WidgetMultiSelection.vue'
import { SelectionWidgetShownKey } from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { ApplicationKind, ArgumentInfoKey } from '@/util/callTree'
import { computed, useTemplateRef } from 'vue'
import type { SuggestionEntryArgument } from 'ydoc-shared/languageServerTypes/suggestions'

const props = defineProps(widgetProps(widgetDefinition))

const currentProject = useCurrentProject()
const graph = computed(() => currentProject.graph.value)
const portInfo = injectPortInfo(true)

const showArgumentValue = computed(() => {
  return (
    portInfo == null ||
    !portInfo.connected ||
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
const connected = computed(() => portInfo?.connected ?? false)
const showArrow = computed(() => {
  // Selection widgets are always above WidgetArgumentName by their priority, so we check them separately.
  const selectionWidgetsShown =
    props.input[SelectionWidgetShownKey] || props.input[MultiSelectionWidgetShownKey]
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
  <div class="WidgetArgumentName" :class="{ primary, missing }">
    <RequiredArgumentArrow
      v-if="showArrow"
      :hide="connected"
      @arrowClick="graph.createEdgeFromPort(props.input.portId, $event)"
    />
    <span class="name">
      <span class="widgetApplyPadding" :class="{ widgetRounded: missing }">{{
        props.input[ArgumentInfoKey].info.name
      }}</span>
    </span>
    <NodeWidget v-if="showArgumentValue" ref="childWidgetRef" :input="innerInput" allowEmpty />
  </div>
</template>

<style scoped>
.WidgetArgumentName {
  display: flex;
  flex-direction: row;
  align-items: center;
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
