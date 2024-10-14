<script lang="ts" setup>
import { computed } from 'vue'

const props = defineProps<{
  data: unknown
}>()

const text = computed(() => JSON.stringify(props.data))
const primitive = computed(() => {
  switch (typeof props.data) {
    case 'string':
      return 'string'
    case 'number':
    case 'bigint':
      return 'number'
    case 'boolean':
      return 'boolean'
    case 'object':
      return 'null'
  }
  return undefined
})
</script>

<template>
  <span class="JsonPrimitive" :class="primitive" v-text="text" />
</template>

<style scoped>
.string {
  color: darkgreen;
  white-space: pre-wrap;
}
.number {
  color: darkgreen;
}
.boolean {
  color: darkgreen;
}
.null {
  color: darkgreen;
}
</style>
