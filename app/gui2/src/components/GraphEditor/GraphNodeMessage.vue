<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

const props = defineProps<{
  message: string
  type: MessageType
}>()
</script>

<script lang="ts">
/** The type of a message. */
export type MessageType = 'error' | 'warning' | 'panic'
export const iconForMessageType: Record<MessageType, Icon> = {
  error: 'error',
  warning: 'warning',
  panic: 'panic',
}
export const colorForMessageType: Record<MessageType, string> = {
  error: 'var(--color-error)',
  warning: 'var(--color-warning)',
  panic: 'var(--color-error)',
}
</script>

<template>
  <div
    class="GraphNodeMessage"
    :class="props.type"
    :style="{ backgroundColor: colorForMessageType[props.type] }"
  >
    <SvgIcon class="icon" :name="iconForMessageType[props.type]" />
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
  user-select: text;
}

.icon {
  margin: auto 0;
}
</style>
