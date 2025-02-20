<script setup lang="ts">
import { useApproach } from '@/composables/animation'
import { Vec2 } from '@/util/data/vec2'
import { computed, shallowRef, watch } from 'vue'

const props = defineProps<{
  position: Vec2 | undefined
  anchor: Vec2 | undefined
  transform: string | undefined
}>()

const hidden = computed(() => props.anchor == null)
const lastSetAnchor = shallowRef<Vec2>()
const lastAnchoredPosition = shallowRef<Vec2>(Vec2.Zero)
watch(
  () => props.anchor,
  (anchor) => {
    if (anchor != null && lastSetAnchor.value !== anchor) {
      lastSetAnchor.value = anchor
    }
  },
)

watch(
  () => [props.anchor, props.position],
  ([anchor, position]) => {
    if (anchor && position) lastAnchoredPosition.value = position
  },
)

const anchorAnimFactor = useApproach(() => (props.anchor != null ? 1 : 0), 60, 0.01)
watch(
  () => props.anchor != null,
  (set) => set && anchorAnimFactor.skip(),
)

const brushStyle = computed(() => {
  if (anchorAnimFactor.value == 0) return null
  const a = lastAnchoredPosition.value
  const anchor = lastSetAnchor.value ?? a
  const b = a.lerp(anchor, anchorAnimFactor.value)
  const dx = Math.abs(a.x - b.x)
  const dy = Math.abs(a.y - b.y)
  if (dx == 0 && dy == 0) return null
  return {
    transform: props.transform,
    left: `${Math.min(a.x, b.x)}px`,
    top: `${Math.min(a.y, b.y)}px`,
    width: `${dx}px`,
    height: `${dy}px`,
  }
})
</script>

<template>
  <div v-if="brushStyle" class="SelectionBrush" :class="{ hidden }" :style="brushStyle"></div>
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
