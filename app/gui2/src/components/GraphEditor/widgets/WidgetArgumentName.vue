<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectPortInfo } from '@/providers/portInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const portInfo = injectPortInfo(true)
const showArgumentValue = computed(() => {
  return (
    ArgumentAst.isInstance(props.input) &&
    (portInfo == null || !portInfo.connected || portInfo.portId !== props.input.ast.astId)
  )
})

const placeholder = computed(() => ArgumentPlaceholder.isInstance(props.input))
const primary = computed(() => props.nesting < 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [ArgumentPlaceholder.isInstance, ArgumentAst.isInstance],
  {
    priority: 1000,
    score: (info) =>
      info.input.info != null && (ArgumentPlaceholder.isInstance(info.input) || info.nesting < 2)
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
