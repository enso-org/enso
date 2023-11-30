<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ApplicationKind, ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const portInfo = injectPortInfo(true)
const showArgumentValue = computed(() => {
  return (
    props.input instanceof ArgumentAst &&
    (portInfo == null || !portInfo.connected || portInfo.portId !== props.input.ast.astId)
  )
})

const placeholder = computed(() => props.input instanceof ArgumentPlaceholder)
const primary = computed(() => props.nesting < 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget([ArgumentPlaceholder, ArgumentAst], {
  priority: 1000,
  score: (props) =>
    props.input.info != null &&
    (props.input instanceof ArgumentPlaceholder ||
      (props.nesting < 2 && props.input.kind === ApplicationKind.Prefix))
      ? Score.Perfect
      : Score.Mismatch,
})
</script>

<template>
  <span class="WidgetArgumentName" :class="{ placeholder, primary }">
    <template v-if="showArgumentValue">
      <span class="value">{{ props.input.info!.name }}</span
      ><NodeWidget :input="props.input" />
    </template>
    <template v-else>{{ props.input.info!.name }}</template>
  </span>
</template>

<style scoped>
.placeholder,
.value {
  color: rgb(255 255 255 / 0.5);
}

.value {
  margin-right: 8px;
}
</style>
