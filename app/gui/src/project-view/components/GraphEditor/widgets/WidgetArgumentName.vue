<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ApplicationKind, ArgumentInfoKey, ArgumentPlaceholder } from '@/util/callTree'
import { computed } from 'vue'
import type { SuggestionEntryArgument } from 'ydoc-shared/languageServerTypes/suggestions'

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

const placeholder = computed(() => props.input instanceof ArgumentPlaceholder)
const primary = computed(() => props.nesting < 2)

const innerInput = computed(() => ({
  ...props.input,
  [ArgumentNameShownKey]: true,
}))
</script>

<script lang="ts">
function hasKnownArgumentName(input: WidgetInput): input is WidgetInput & {
  value: Ast.Ast | string | undefined
  [ArgumentInfoKey]: { info: SuggestionEntryArgument }
} {
  return !WidgetInput.isToken(input) && input[ArgumentInfoKey]?.info != null
}

export const widgetDefinition = defineWidget(
  hasKnownArgumentName,
  {
    priority: 100,
    score: (props) => {
      const isPlaceholder = !(props.input.value instanceof Ast.Ast)
      const isTopArg =
        props.nesting < 2 && props.input[ArgumentInfoKey].appKind === ApplicationKind.Prefix
      return isPlaceholder || isTopArg ? Score.Perfect : Score.Mismatch
    },
  },
  import.meta.hot,
)

export const ArgumentNameShownKey: unique symbol = Symbol.for('WidgetInput:ArgumentNameShown')
</script>

<template>
  <div class="WidgetArgumentName" :class="{ placeholder, primary }">
    <span class="name widgetApplyPadding">{{ props.input[ArgumentInfoKey].info.name }}</span>
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

.placeholder,
.name {
  color: rgb(255 255 255 / 0.5);
}
</style>
