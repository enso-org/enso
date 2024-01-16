<script setup lang="ts">
import { useApproach } from '@/composables/animation'
import type { Vec2 } from '@/util/data/vec2'
import { computed, watch, type Ref } from 'vue'

const props = defineProps<{
  position: Vec2
  anchor: Vec2 | undefined
}>()

const hidden = computed(() => props.anchor == null)
const lastSetAnchor: Ref<Vec2 | undefined> = computed(() => props.anchor ?? lastSetAnchor.value)

const anchorAnimFactor = useApproach(() => (props.anchor != null ? 1 : 0), 60)
watch(
  () => props.anchor != null,
  (set) => set && anchorAnimFactor.skip(),
)

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
  <div class="SelectionBrush" :class="{ hidden }" :style="brushStyle"></div>
</template>

<style scoped>
.SelectionBrush {
  --margin-brush: 6px;
  transition-property: border, margin;
  transition-duration: 100ms;
  box-sizing: content-box;
  position: absolute;
  pointer-events: none;
  background: lch(70% 0 0 / 0.5);
  border-radius: 8px;
  border: var(--margin-brush) solid #0000;
  margin: calc(0px - var(--margin-brush));
  z-index: 1000;

  &.hidden {
    /* Keep brush "displayed" for animations */
    display: block;
    --margin-brush: 0px;
  }
}
</style>
