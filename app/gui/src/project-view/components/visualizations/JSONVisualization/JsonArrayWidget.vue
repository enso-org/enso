<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import type { Opt } from '@/util/data/opt'
import { computed } from 'vue'
import type { CreateProjection } from './types'

const props = defineProps<{
  data: unknown[]
  indent: string
  createProjectionCb?: Opt<CreateProjection>
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)
const nextIndent = computed(() => (block.value ? props.indent + '  ' : ''))

function entryTitle(index: number) {
  const singleEntry = `Click to create a node selecting element ${index} of the array.`
  return props.data.length > 1 ?
      `${singleEntry} Shift-click to create nodes selecting all ${props.data.length} elements.`
    : singleEntry
}

function onClick(index: number, event: MouseEvent) {
  if (props.createProjectionCb) {
    props.createProjectionCb([event.shiftKey ? [...props.data.keys()] : [index]])
    event.stopPropagation()
  }
}
</script>

<template>
  <span class="JsonArrayWidget" :class="{ block }">
    [
    <span
      v-for="(child, index) in data"
      :key="index"
      :title="createProjectionCb != null ? entryTitle(index) : ''"
      class="element"
      :class="{ clickable: createProjectionCb != null }"
      @click="onClick(index, $event)"
    >
      <pre class="indent" v-text="nextIndent" />
      <JsonValueWidget
        :data="child"
        :indent="nextIndent"
        :createProjectionCb="
          createProjectionCb && ((path) => createProjectionCb?.([[index], ...path]))
        "
      />{{
        // This newline is needed for copying text.
        index === data.length - 1 ? '\n'
        : block ? ','
        : ', '
      }}
    </span>
    <span><pre class="indent" v-text="indent" />]</span>
  </span>
</template>

<style scoped>
.block > .element {
  display: block;
}
.indent {
  display: inline;
}
</style>
