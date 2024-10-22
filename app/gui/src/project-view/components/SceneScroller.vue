<script setup lang="ts">
import ScrollControl, { type ScrollbarEvent } from '@/components/ScrollBar.vue'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'

const props = defineProps<{
  navigator: GraphNavigator
  scrollableArea: Rect
}>()

const scrollbarState = computed(() => scrollingState.value ?? scrollInputs.value)
const scrollInputs = computed(() => ({
  scale: props.navigator.scale,
  range: props.scrollableArea.size.scale(props.navigator.scale),
  pos: props.navigator.viewport.pos.sub(props.scrollableArea.pos).scale(props.navigator.scale),
}))
const scrollingState = ref<{
  /** Current scrollbar position, as offset in client units from the origin of the scrollable area. */
  pos: Vec2
  /** Zoom factor when scrolling started. */
  readonly scale: number
  /** Scrollbar range, in client units, when scrolling started. */
  readonly range: Vec2
  /** `pos` when scrolling started. */
  readonly scrollStartPos: Vec2
  /** Viewport center, in scene coordinates, when scrolling started. */
  readonly scrollOrigin: Vec2
}>()

function scroll(event: ScrollbarEvent) {
  switch (event.type) {
    case 'start': {
      const scrollStartPos = scrollInputs.value.pos
      const scrollOrigin = props.navigator.viewport.center()
      scrollingState.value = { ...scrollInputs.value, scrollStartPos, scrollOrigin }
      break
    }
    case 'move': {
      if (!scrollingState.value) return
      scrollingState.value.pos = scrollingState.value.scrollStartPos.add(event.startOffset)
      props.navigator.scrollTo(
        scrollingState.value.scrollOrigin.addScaled(
          event.startOffset,
          1 / scrollingState.value.scale,
        ),
      )
      break
    }
    case 'stop': {
      scrollingState.value = undefined
      break
    }
    case 'jump': {
      const proportionalPos = event.position / scrollInputs.value.range.getAxis(event.axis)
      const scaledPos = proportionalPos * props.scrollableArea.size.getAxis(event.axis)
      const pos = scaledPos + props.scrollableArea.pos.getAxis(event.axis)
      props.navigator.scrollTo(props.navigator.viewport.center().setAxis(event.axis, pos))
      break
    }
  }
}
</script>

<template>
  <ScrollControl
    :size="scrollbarState.range"
    :position="scrollbarState.pos"
    @scroll="scroll($event)"
  />
</template>
