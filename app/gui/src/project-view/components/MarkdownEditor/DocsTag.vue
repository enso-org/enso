<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  label: string
  value?: string | undefined
  class?: string | undefined
}>()

const text = computed(() => {
  if (props.value == null || props.value.length === 0) return props.label
  return `${props.label}: ${props.value}`
})
</script>

<template>
  <div :class="`DocsTag ${props.class ?? ''}`" v-text="text"></div>
</template>

<style scoped>
.DocsTag {
  flex: 0 0 auto;
  height: 24px;
  color: rgba(0, 0, 0, 0.6);
  background-color: #dcd8d8;
  border-radius: 4px;
  padding: 1px 5px;
}

.group {
  background-color: var(--enso-docs-group-color, #5f5e5e);
  color: #fff;
}

.unstable {
  background-color: #e85252;
  color: #fff;
}

.advanced {
  background-color: #e89d51;
  color: #fff;
}

.deprecated {
  background-color: #e89d52;
  color: #fff;
}
</style>
