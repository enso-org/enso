<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

const props = defineProps<{
  message: string
  type: MessageType
}>()

function copyText() {
  window.navigator.clipboard.writeText(props.message)
}
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
  <div class="GraphNodeMessage" :style="{ '--background-color': colorForMessageType[props.type] }">
    <SvgIcon class="icon" :name="iconForMessageType[props.type]" />
    <div class="message" v-text="props.message"></div>
    <div class="toolbar">
      <SvgButton name="copy2" class="copyButton" title="Copy message text" @click.stop="copyText" />
    </div>
  </div>
</template>

<style scoped>
.GraphNodeMessage {
  --horizontal-padding: 8px;
  display: flex;
  height: 24px;
  padding: 0 var(--horizontal-padding);
  align-items: flex-start;
  gap: 6px;
  font-weight: 800;
  white-space: nowrap;
  border-radius: var(--radius-full);
  color: var(--color-text-inversed);
  background-color: var(--background-color);
  line-height: 20px;
}

.icon {
  margin: auto 0;
}

.message {
  margin-top: 1px;
}

.toolbar {
  padding: 4px;
  margin-right: calc(0px - var(--horizontal-padding));
  border-radius: var(--radius-full);
  position: relative;
  z-index: 1;

  & > .SvgButton:hover {
    background-color: color-mix(in oklab, black, transparent 90%);
    color: color-mix(in oklab, var(--color-text-inversed), transparent 20%);
  }

  & > .SvgButton:active {
    background-color: color-mix(in oklab, black, transparent 70%);
  }
}

.copyButton:active {
  color: var(--color-text-inversed);
}
</style>
