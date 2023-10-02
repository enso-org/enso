<script setup lang="ts">
import { computed, ref, watch, type Ref } from 'vue'

import { useApproach } from '@/util/animation'
import { useDocumentEvent } from '@/util/events'
import type { Vec2 } from '@/util/vec2'

const props = defineProps<{
  position: Vec2
  anchor: Vec2 | undefined
}>()

const hidden = ref(false)
const lastSetAnchor: Ref<Vec2 | undefined> = computed(() => props.anchor ?? lastSetAnchor.value)

const anchorAnimFactor = useApproach(() => (props.anchor != null ? 1 : 0), 60)
watch(
  () => props.anchor != null,
  (set) => set && anchorAnimFactor.skip(),
)

let lastEventTarget: Element | null
useDocumentEvent('mouseover', (event) => {
  if (event.target instanceof Element) {
    if (event.target === lastEventTarget) {
      return
    }
    lastEventTarget = event.target
    hidden.value = getComputedStyle(event.target).cursor !== 'none'
  }
})

const brushStyle = computed(() => {
  const a = props.position
  const anchor = lastSetAnchor.value ?? a
  const b = a.lerp(anchor, anchorAnimFactor.value)

  return {
    left: `${Math.min(a.x, b.x)}px`,
    top: `${Math.min(a.y, b.y)}px`,
    width: `${Math.abs(a.x - b.x)}px`,
    height: `${Math.abs(a.y - b.y)}px`,
  }
})
</script>

<template>
  <div
    class="SelectionBrush"
    :class="{ cursor: props.anchor == null, hidden }"
    :style="brushStyle"
  ></div>
</template>

<style scoped>
.SelectionBrush {
  --radius-cursor: 8px;
  --margin-brush: 6px;
  transition-property: border, margin;
  transition-duration: 100ms;
  box-sizing: content-box;
  position: absolute;
  pointer-events: none;
  background: lch(70% 0 0 / 0.5);
  border-radius: var(--radius-cursor);
  border: var(--margin-brush) solid #0000;
  margin: calc(0px - var(--margin-brush));
  z-index: 1000;

  &.hidden {
    display: none;
  }

  &.cursor {
    border: var(--radius-cursor) solid #0000;
    margin: calc(0px - var(--radius-cursor));

    /* &.transition { */
    /* transition-property: left, top, width, height; */
    /* transition-duration: 150ms; */
    /* } */
  }
}
</style>
