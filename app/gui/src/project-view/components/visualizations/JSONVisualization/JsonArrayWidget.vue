<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import type { Opt } from '@/util/data/opt'
import { computed } from 'vue'
import type { CreateProjection } from './types'

const props = defineProps<{
  data: unknown[]
  createProjectionCb?: Opt<CreateProjection>
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)

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
    <span
      v-for="(child, index) in props.data"
      :key="index"
      :title="createProjectionCb != null ? entryTitle(index) : ''"
      class="element"
      :class="{ clickable: createProjectionCb != null }"
      @click="onClick(index, $event)"
    >
      <JsonValueWidget
        :data="child"
        :createProjectionCb="
          createProjectionCb && ((path) => createProjectionCb?.([[index], ...path]))
        "
      />
    </span>
  </span>
</template>

<style scoped>
.JsonArrayWidget {
  &::before {
    display: inline;
    content: '[';
  }
  &::after {
    display: inline;
    content: ']';
  }
}
.block > .element {
  display: block;
  margin-left: 1em;
}
.element:not(:last-child)::after {
  display: inline;
  content: ', ';
}
.block > .element:not(:last-child)::after {
  content: ',';
}
</style>
