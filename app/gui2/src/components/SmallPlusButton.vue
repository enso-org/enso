<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SvgIcon from '@/components/SvgIcon.vue'

const emit = defineEmits<{
  createNodes: [options: NodeCreationOptions[]]
}>()

function addNode() {
  emit('createNodes', [{ commit: false, content: undefined }])
}
</script>

<template>
  <div class="SmallPlusButton add-node" @click.stop @pointerdown.stop @pointerup.stop>
    <SvgIcon name="add" class="icon button" @click.stop="addNode" />
  </div>
</template>

<style scoped>
.SmallPlusButton {
  width: var(--node-height);
  height: var(--node-height);
  &:before {
    content: '';
    position: absolute;
    left: 0;
    top: 0;
    backdrop-filter: var(--blur-app-bg);
    background: var(--color-app-bg);
    border-radius: 16px;
    width: var(--node-height);
    height: var(--node-height);
  }
  &:hover:before {
    background: rgb(230, 230, 255);
  }
  &:active:before {
    background: rgb(158, 158, 255);
  }
}

.icon {
  display: inline-flex;
  background: none;
  margin: 8px;
  padding: 0;
  border: none;
  opacity: 30%;
}
</style>
