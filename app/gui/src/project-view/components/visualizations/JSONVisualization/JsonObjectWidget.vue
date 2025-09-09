<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { Opt } from '@/util/data/opt'
import { computed } from 'vue'
import { CreateProjection } from './types'

const props = defineProps<{ data: object; createProjectionCb?: Opt<CreateProjection> }>()

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

function onClick(key: string, event: MouseEvent) {
  if (props.createProjectionCb) {
    props.createProjectionCb([event.shiftKey ? Object.keys(props.data) : [key]])
    event.stopPropagation()
  }
}
</script>

<template>
  <span class="JsonObjectWidget" :class="{ block }">
    <span
      v-for="[key, value] in Object.entries(props.data)"
      :key="key"
      :title="createProjectionCb != null ? entryTitle(key) : ''"
      class="field"
      :class="{ clickable: createProjectionCb != null }"
      @click.stop="onClick(key, $event)"
    >
      <span class="key" v-text="JSON.stringify(key)" />:
      <JsonValueWidget
        :data="value"
        :createProjectionCb="
          createProjectionCb && ((path) => createProjectionCb?.([[key], ...path]))
        "
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
}
.viewonly .key {
  color: darkred;
  text-decoration: none;
}
</style>
