<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { isRequiredArgument } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { ApplicationKind, ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'
import { type SuggestionEntryArgument } from 'ydoc-shared/languageServerTypes/suggestions'

const props = defineProps(widgetProps(widgetDefinition))

const portInfo = injectPortInfo(true)
const showArgumentValue = computed(() => {
  return (
    !WidgetInput.isAst(props.input) ||
    portInfo == null ||
    !portInfo.connected ||
    (portInfo.portId as string) !== (props.input.value.id as string)
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
    <span class="name">
      <span class="widgetApplyPadding" :class="{ widgetRounded: missing }">{{
        props.input[ArgumentInfoKey].info.name
      }}</span>
    </span>
    <NodeWidget v-if="showArgumentValue" :input="innerInput" allowEmpty />
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
