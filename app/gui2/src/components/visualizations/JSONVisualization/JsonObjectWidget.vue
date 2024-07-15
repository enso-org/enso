<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { computed } from 'vue'

const props = defineProps<{ data: object }>()
const emit = defineEmits<{
  createProjection: [path: (string | number)[][]]
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)

const escapedKeys = computed(() =>
  Array.from(Object.keys(props.data), (key) => JSON.stringify(key)),
)

function entryTitle(key: string) {
  const singleEntry = `Click to create a node selecting the ${JSON.stringify(key)} field.`
  if (Object.keys(props.data).length > 1)
    return `${singleEntry} Shift-click to create nodes selecting all fields of the object (${escapedKeys.value.join(', ')}).`
  else return singleEntry
}
</script>

<template>
  <span class="JsonObjectWidget" :class="{ block }">
    <span
      v-for="[key, value] in Object.entries(props.data)"
      :key="key"
      :title="entryTitle(key)"
      class="field clickable"
      @click.stop="emit('createProjection', [$event.shiftKey ? Object.keys(props.data) : [key]])"
    >
      <span class="key" v-text="JSON.stringify(key)" />:
      <JsonValueWidget
        :data="value"
        @createProjection="emit('createProjection', [[key], ...$event])"
      />
    </span>
  </span>
</template>

<style scoped>
.JsonObjectWidget {
  &::before {
    display: inline;
    content: '{ ';
  }
  &::after {
    display: inline;
    content: ' }';
  }
}
.JsonObjectWidget.block {
  &::before {
    content: '{';
  }
  &::after {
    content: '}';
  }
}
.block > .field {
  display: block;
  margin-left: 1em;
}
.field:not(:last-child)::after {
  display: inline;
  content: ', ';
}
.block > .field:not(:last-child)::after {
  content: ',';
}
.key {
  color: blue;
  text-decoration: underline;
}
.viewonly .key {
  color: darkred;
  text-decoration: none;
}
</style>
