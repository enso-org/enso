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
    props.input instanceof ArgumentPlaceholder ||
    portInfo == null ||
    !portInfo.connected ||
    (portInfo.portId as string) !== (props.input.ast.exprId as string)
  )
})

const placeholder = computed(() => props.input instanceof ArgumentPlaceholder)
const primary = computed(() => props.nesting < 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget([ArgumentAst.matchWithArgInfo, ArgumentPlaceholder], {
  priority: 1000,
  score: (props) => {
    const isPlaceholder = props.input instanceof ArgumentPlaceholder
    const isTopArg = props.nesting < 2 && props.input.kind === ApplicationKind.Prefix
    return isPlaceholder || isTopArg ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetArgumentName" :class="{ placeholder, primary }">
    <template v-if="showArgumentValue">
      <span class="name">{{ props.input.argInfo.name }}</span
      ><NodeWidget :input="props.input" allowEmpty />
    </template>
    <template v-else>{{ props.input.argInfo.name }}</template>
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
