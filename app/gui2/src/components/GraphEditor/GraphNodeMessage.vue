<script setup lang="ts">
/// types of messages
export type GraphNodeMessageType = 'error' | 'warning'

const props = defineProps<{ message: string; type: GraphNodeMessageType }>()

function styleClassForType(type: GraphNodeMessageType) {
  switch (type) {
    case 'error':
      return 'GraphNodeError'
    case 'warning':
      return 'GraphNodeWarning'
    default:
      return ''
  }
}
</script>

<template>
  <div class="GraphNodeMessage" :class="styleClassForType(props.type)" v-text="props.message"></div>
</template>

<style scoped>
.GraphNodeMessage {
  display: flex;
  height: 24px;
  padding: 1px 8px;
  align-items: flex-start;
  gap: 10px;
  font-weight: 800;
  white-space: nowrap;
  border-radius: var(--radius-full);
  color: var(--color-text-inversed);
  line-height: 20px;
}

.GraphNodeWarning {
  background-color: #faa212;
}

.GraphNodeError {
  background-color: #e85252;
}
</style>
