<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Vec2 } from '@/util/data/vec2'
import { ref } from 'vue'

const emit = defineEmits<{
  createNode: [options: NodeCreationOptions]
}>()

const navigator = injectGraphNavigator(true)

const addNodeButton = ref<HTMLElement>()

function addNode() {
  const clientRect = addNodeButton.value?.getBoundingClientRect()
  const position =
    clientRect && navigator?.clientToScenePos(new Vec2(clientRect.left, clientRect.top))
  emit('createNode', { position, commit: false, content: undefined })
}
</script>

<template>
  <div
    ref="addNodeButton"
    class="SmallPlusButton add-node"
    @click.stop
    @pointerdown.stop
    @pointerup.stop
  >
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
