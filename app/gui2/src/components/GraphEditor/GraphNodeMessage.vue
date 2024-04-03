<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

const props = defineProps<{
  message: string
  type: GraphNodeMessageType
}>()
</script>

<script lang="ts">
type MessageType = 'error' | 'warning' | 'panic'
/** The type of a message. */
export class GraphNodeMessageType {
  private constructor(private readonly type: MessageType) {}

  static Error: GraphNodeMessageType = new GraphNodeMessageType('error')
  static Warning: GraphNodeMessageType = new GraphNodeMessageType('warning')
  static Panic: GraphNodeMessageType = new GraphNodeMessageType('panic')

  get iconName() {
    return iconForMessageType[this.type]
  }

  get cssColor() {
    return colorForMessageType[this.type]
  }
}
const iconForMessageType: Record<MessageType, Icon> = {
  error: 'error',
  warning: 'warning',
  panic: 'panic',
}
const colorForMessageType: Record<MessageType, string> = {
  error: 'var(--color-error)',
  warning: 'var(--color-warning)',
  panic: 'var(--color-error)',
}
</script>

<template>
  <div
    class="GraphNodeMessage"
    :class="props.type"
    :style="{ backgroundColor: props.type.cssColor }"
  >
    <SvgIcon class="icon" :name="props.type.iconName" />
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

.icon {
  margin: auto 0;
}
</style>
