<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { isInstance } from '@/util/predicates'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const portInfo = injectPortInfo(true)
const showArgumentValue = computed(() => {
  return (
    isInstance(ArgumentAst, props.input) &&
    (portInfo == null || !portInfo.connected || portInfo.portId !== props.input.ast.astId)
  )
})

const placeholder = computed(() => isInstance(ArgumentPlaceholder, props.input))
const primary = computed(() => props.nesting < 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [isInstance(ArgumentPlaceholder), isInstance(ArgumentAst)],
  {
    priority: 1000,
    score: (props) =>
      props.input.info != null &&
      (isInstance(ArgumentPlaceholder, props.input) || props.nesting < 2)
        ? Score.Perfect
        : Score.Mismatch,
  },
)
</script>

<template>
  <span class="WidgetArgumentName" :class="{ placeholder, primary }">
    <template v-if="showArgumentValue">
      <span class="name">{{ props.input.info!.name }}</span
      ><NodeWidget :input="props.input" />
    </template>
    <template v-else>{{ props.input.info!.name }}</template>
  </span>
</template>

<style scoped>
.placeholder,
.name {
  color: rgb(255 255 255 / 0.5);
}

.name {
  margin-right: 8px;
}
</style>
