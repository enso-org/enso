<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ApplicationKind, Argument } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const portInfo = injectPortInfo(true)
const showArgumentValue = computed(() => {
  return (
    props.input.ast &&
    (portInfo == null || !portInfo.connected || portInfo.portId !== props.input.ast.astId)
  )
})

const placeholder = computed(() => props.input.isPlaceholder())
const primary = computed(() => props.nesting < 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(Argument, {
  priority: 1000,
  score: (props) => {
    const isPlaceholderName = props.input.argInfo != null && props.input.isPlaceholder()
    const isTopName = props.nesting < 2 && props.input.kind === ApplicationKind.Prefix
    return isPlaceholderName || isTopName ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <span class="WidgetArgumentName" :class="{ placeholder, primary }">
    <template v-if="showArgumentValue">
      <span class="value">{{ props.input.argInfo?.name }}</span
      ><NodeWidget :input="props.input" />
    </template>
    <template v-else>{{ props.input.argInfo?.name }}</template>
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
