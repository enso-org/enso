<script lang="ts" setup>
import { Vec2 } from '@/util/data/vec2'
import type { AstId } from 'shared/ast'
import { computed, ref, watchEffect } from 'vue'

const props = defineProps<{
  nodePosition: Vec2
  nodeSize: Vec2
  nodeHovered: boolean
  nodeId: AstId
  selected: boolean
  color: string
}>()

const emit = defineEmits<{
  visible: [boolean]
}>()

const hovered = ref(false)
const visible = computed(() => props.selected || hovered.value || props.nodeHovered)

watchEffect(() => emit('visible', visible.value))

const transform = computed(() => {
  const { x, y } = props.nodePosition
  return `translate(${x}px, ${y}px)`
})
const nodeWidthPx = computed(() => `${props.nodeSize.x}px`)
const nodeHeightPx = computed(() => `${props.nodeSize.y}px`)
</script>

<template>
  <div
    class="GraphNodeSelection"
    :class="{ visible, selected: props.selected }"
    :style="{ transform }"
    :data-node-id="props.nodeId"
    @pointerenter="hovered = true"
    @pointerleave="hovered = false"
  />
</template>

<style scoped>
.GraphNodeSelection {
  position: absolute;
  inset: calc(0px - var(--selected-node-border-width));
  width: calc(var(--selected-node-border-width) * 2 + v-bind('nodeWidthPx'));
  height: calc(var(--selected-node-border-width) * 2 + v-bind('nodeHeightPx'));

  &:before {
    position: absolute;
    content: '';
    opacity: 0.2;
    display: block;
    inset: var(--selected-node-border-width);
    box-shadow: 0 0 0 calc(0px - var(--node-border-radius)) v-bind('props.color');
    border-radius: var(--node-border-radius);

    transition:
      box-shadow 0.2s ease-in-out,
      opacity 0.2s ease-in-out;
  }
}

.GraphNodeSelection.visible::before {
  box-shadow: 0 0 0 var(--selected-node-border-width) v-bind('props.color');
}

.GraphNodeSelection:not(.selected):hover::before {
  opacity: 0.3;
}
</style>
