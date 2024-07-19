<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { computed } from 'vue'

const props = defineProps<{ data: unknown[] }>()
const emit = defineEmits<{
  createProjection: [path: (string | number)[][]]
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)

function entryTitle(index: number) {
  const singleEntry = `Click to create a node selecting element ${index} of the array.`
  return props.data.length > 1 ?
      `${singleEntry} Shift-click to create nodes selecting all ${props.data.length} elements.`
    : singleEntry
}
</script>

<template>
  <span class="JsonArrayWidget" :class="{ block }">
    <span
      v-for="(child, index) in props.data"
      :key="index"
      :title="entryTitle(index)"
      class="element clickable"
      @click.stop="emit('createProjection', [$event.shiftKey ? [...props.data.keys()] : [index]])"
    >
      <JsonValueWidget
        :data="child"
        @createProjection="emit('createProjection', [[index], ...$event])"
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
