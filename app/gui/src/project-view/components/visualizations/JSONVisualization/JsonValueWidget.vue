<script lang="ts" setup>
import JsonArrayWidget from '@/components/visualizations/JSONVisualization/JsonArrayWidget.vue'
import JsonErrorWidget from '@/components/visualizations/JSONVisualization/JsonErrorWidget.vue'
import JsonObjectWidget from '@/components/visualizations/JSONVisualization/JsonObjectWidget.vue'
import JsonPrimitiveWidget from '@/components/visualizations/JSONVisualization/JsonPrimitiveWidget.vue'

const props = defineProps<{ data: unknown }>()
const emit = defineEmits<{
  createProjection: [path: (string | number)[][]]
}>()
</script>

<template>
  <JsonErrorWidget
    v-if="props.data && typeof props.data === 'object' && '_to_js_object_error_' in props.data"
    :data="props.data._to_js_object_error_"
  />
  <JsonArrayWidget
    v-else-if="Array.isArray(props.data)"
    :data="props.data"
    @createProjection="emit('createProjection', $event)"
  />
  <JsonPrimitiveWidget
    v-else-if="
      props.data && typeof props.data === 'object' && (props.data as any).type === 'Nothing'
    "
    :data="null"
  />
  <JsonObjectWidget
    v-else-if="props.data && typeof props.data === 'object'"
    :data="props.data"
    @createProjection="emit('createProjection', $event)"
  />
  <JsonPrimitiveWidget v-else :data="props.data" />
</template>
