<script setup lang="ts">
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import type { AiPending } from '@/stores/aiPrompts'
import { computed } from 'vue'

const { pending } = defineProps<{ pending: AiPending }>()
const emit = defineEmits<{ cancel: [] }>()

const transform = computed(() => {
  const { x, y } = pending.position
  return `translate(${x}px, ${y}px)`
})

function onKeydown(event: KeyboardEvent) {
  if (event.key === 'Backspace' || event.key === 'Delete') {
    event.preventDefault()
    event.stopPropagation()
    emit('cancel')
  }
}
</script>

<template>
  <div
    class="AiPendingNode"
    :class="{ failed: pending.status === 'failed' }"
    :style="{ transform }"
    tabindex="0"
    @keydown="onKeydown"
    @pointerdown.stop
    @click.stop
  >
    <div class="bubble" data-testid="ai-pending-status">
      {{ pending.statusText }}
    </div>
    <div class="skeleton">
      <GrowingSpinner
        v-if="pending.status !== 'failed'"
        class="spinner"
        :size="16"
        phase="loading-medium"
      />
      <span class="prompt">AI: {{ pending.prompt }}</span>
      <button
        type="button"
        class="cancel"
        title="Cancel AI prompt (Backspace)"
        aria-label="Cancel AI prompt"
        @click.stop="emit('cancel')"
      >
        ×
      </button>
    </div>
  </div>
</template>

<style scoped>
.AiPendingNode {
  position: absolute;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  z-index: 2;
  outline: none;
}

.bubble {
  position: absolute;
  bottom: calc(100% + var(--node-vertical-gap, 4px));
  left: 0;
  max-width: 800px;
  padding: 4px 10px;
  border-radius: 12px;
  background-color: var(--node-color-no-type, #777);
  color: var(--color-text-inversed, #fff);
  font-size: 12px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  opacity: 0.85;
  pointer-events: none;
}

.skeleton {
  display: flex;
  align-items: center;
  gap: 8px;
  min-height: var(--node-base-height, 32px);
  padding: 6px 12px;
  border-radius: var(--node-border-radius, 16px);
  background-color: var(--color-node-background-pending, #d8d8d8);
  color: var(--color-text, #222);
  font-family: var(--font-code, monospace);
  opacity: 0.85;
}

.AiPendingNode:focus .skeleton {
  outline: 2px solid var(--color-focus-ring, #1976d2);
  outline-offset: 2px;
}

.AiPendingNode.failed .skeleton {
  background-color: var(--color-node-background-error, #f8d7da);
}

.spinner {
  flex: 0 0 auto;
}

.prompt {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 320px;
}

.cancel {
  flex: 0 0 auto;
  width: 20px;
  height: 20px;
  border-radius: 50%;
  border: none;
  background: rgba(0, 0, 0, 0.08);
  color: inherit;
  cursor: pointer;
  font-size: 14px;
  line-height: 1;
  padding: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
.cancel:hover {
  background: rgba(0, 0, 0, 0.18);
}
</style>
