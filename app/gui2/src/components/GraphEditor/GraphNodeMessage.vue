<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed } from 'vue'

/** The type of a message. */
export type GraphNodeMessageType = 'error' | 'warning'

const props = defineProps<{
  message: string
  type: GraphNodeMessageType
  icon?: Icon
}>()

const icon = computed(() => iconForType[props.type])
</script>

<script lang="ts">
const styleClassForType: Record<GraphNodeMessageType, string> = {
  error: 'GraphNodeError',
  warning: 'GraphNodeWarning',
}

const iconForType: Record<GraphNodeMessageType, Icon | undefined> = {
  error: 'error',
  warning: 'warning',
}
</script>

<template>
  <div class="GraphNodeMessage" :class="styleClassForType[props.type]">
    <SvgIcon v-if="icon" class="icon" :name="icon" />
    <div v-text="props.message"></div>
  </div>
</template>

<style scoped>
.GraphNodeMessage {
  display: flex;
  height: 24px;
  padding: 1px 8px;
  align-items: flex-start;
  gap: 6px;
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

.icon {
  margin: auto 0;
}
</style>
