<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { computed } from 'vue'

const props = defineProps<{ data: unknown[] }>()
const emit = defineEmits<{
  createProjection: [path: (string | number)[][]]
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)
</script>

<template>
  <span class="JsonArrayWidget" :class="{ block }" @pointerdown.stop @pointerup.stop @click.stop>
    <span
      v-for="(child, index) in props.data"
      :key="index"
      :title="`Click to create a node selecting element ${index} of the array.`"
      class="button element"
      @click="emit('createProjection', [[index]])"
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
