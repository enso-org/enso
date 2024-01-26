<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ApplicationKind, ArgumentInfoKey, ArgumentPlaceholder } from '@/util/callTree'
import type { SuggestionEntryArgument } from 'shared/languageServerTypes/suggestions'
import { computed } from 'vue'

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
</script>

<script lang="ts">
function hasKnownArgumentName(input: WidgetInput): input is WidgetInput & {
  value: Ast.Ast | string | undefined
  [ArgumentInfoKey]: { info: SuggestionEntryArgument }
} {
  return !WidgetInput.isToken(input) && input[ArgumentInfoKey]?.info != null
}

export const widgetDefinition = defineWidget(hasKnownArgumentName, {
  priority: 1000,
  score: (props) => {
    const isPlaceholder = !(props.input.value instanceof Ast.Ast)
    const isTopArg =
      props.nesting < 2 && props.input[ArgumentInfoKey].appKind === ApplicationKind.Prefix
    return isPlaceholder || isTopArg ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetArgumentName" :class="{ placeholder, primary }">
    <template v-if="showArgumentValue">
      <span class="name">{{ props.input[ArgumentInfoKey].info.name }}</span
      ><NodeWidget :input="props.input" allowEmpty />
    </template>
    <template v-else>{{ props.input[ArgumentInfoKey].info.name }}</template>
  </div>
</template>

<style scoped>
.WidgetArgumentName {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.placeholder,
.name {
  color: rgb(255 255 255 / 0.5);
  margin-right: 8px;

  &:last-child {
    margin-right: 0px;
  }
}
</style>
