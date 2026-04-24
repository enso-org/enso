<script lang="ts" setup>
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import type { Opt } from '@/util/data/opt'
import { computed } from 'vue'
import type { CreateProjection } from './types'

const props = defineProps<{
  data: object
  indent: string
  createProjectionCb?: Opt<CreateProjection>
}>()

const MAX_INLINE_LENGTH = 40

const block = computed(() => JSON.stringify(props.data).length > MAX_INLINE_LENGTH)
const nextIndent = computed(() => (block.value ? props.indent + '  ' : ''))

const escapedKeys = computed(() =>
  Array.from(Object.keys(props.data), (key) => JSON.stringify(key)),
)

const entries = computed(() => Object.entries(props.data))

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
    <span>{</span>
    <span
      v-for="([key, value], index) in entries"
      :key="key"
      :title="createProjectionCb != null ? entryTitle(key) : ''"
      class="field"
      :class="{ clickable: createProjectionCb != null }"
      @click.stop="onClick(key, $event)"
    >
      <pre class="indent" v-text="nextIndent" />
      <span class="key" v-text="JSON.stringify(key)" />:
      <JsonValueWidget
        :data="value"
        :indent="nextIndent"
        :createProjectionCb="
          createProjectionCb && ((path) => createProjectionCb?.([[key], ...path]))
        "
      />{{
        // This newline is needed for copying text.
        index === entries.length - 1 ? '\n'
        : block ? ','
        : ', '
      }}
    </span>
    <span><pre class="indent" v-text="indent"></pre>}</span>
  </span>
</template>

<style scoped>
.block > .field {
  display: block;
}
.key {
  color: blue;
}
.viewonly .key {
  color: darkred;
  text-decoration: none;
}
.indent {
  display: inline;
}
</style>
