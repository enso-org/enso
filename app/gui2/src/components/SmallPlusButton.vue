<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Vec2 } from '@/util/data/vec2'
import { ref } from 'vue'

const emit = defineEmits<{
  createNodes: [options: NodeCreationOptions[]]
}>()

const navigator = injectGraphNavigator(true)

const addNodeButton = ref<HTMLElement>()

function addNode() {
  const clientRect = addNodeButton.value?.getBoundingClientRect()
  const position =
    clientRect && navigator?.clientToScenePos(new Vec2(clientRect.left, clientRect.top))
  emit('createNodes', [{ position, commit: false, content: undefined }])
}
</script>

<template>
  <div
    ref="addNodeButton"
    class="SmallPlusButton add-node button"
    @click.stop="addNode"
    @pointerdown.stop
    @pointerup.stop
  >
    <SvgIcon name="add" class="icon" />
  </div>
</template>

<style scoped>
.SmallPlusButton {
  width: var(--node-height);
  height: var(--node-height);

  backdrop-filter: var(--blur-app-bg);
  background: var(--color-app-bg);
  border-radius: 16px;

  &:hover {
    background: rgb(230, 230, 255);
  }
  &:active {
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
