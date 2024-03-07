<script setup lang="ts">
import { usePointer, useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, watchEffect } from 'vue'

const element = ref<HTMLElement>()

const props = defineProps<{
  /** The size of the scrollable area, in client pixels. */
  size: Vec2
  /** The scrollbar's offset from the top-left of the scrollable area, in client pixels. */
  position: Vec2
}>()

const emit = defineEmits<{
  scroll: [event: ScrollbarEvent]
}>()

const BAR_END_MARGIN = 2
const BAR_WIDTH = 6

const viewportSize = useResizeObserver(element)

const range = computed(
  () =>
    Rect.Bounding(new Rect(Vec2.Zero, props.size), new Rect(props.position, viewportSize.value))
      .size,
)

const xStart = ref('')
const yStart = ref('')
const xLength = ref('')
const yLength = ref('')
const xFull = ref(false)
const yFull = ref(false)

watchEffect(() => {
  const viewportFraction = Vec2.DotProduct(viewportSize.value, range.value.reciprocal())
  const barStartFraction = Vec2.DotProduct(props.position, range.value.reciprocal())
  const trackLength = viewportSize.value.sub(
    new Vec2(BAR_END_MARGIN * 2 + BAR_WIDTH, BAR_END_MARGIN * 2 + BAR_WIDTH),
  )
  const barStart = Vec2.DotProduct(trackLength, barStartFraction).max(Vec2.Zero)
  const barLength = Vec2.DotProduct(trackLength, viewportFraction)
  const barEnd = barStart.add(barLength).min(trackLength)
  xStart.value = `${barStart.x}px`
  xLength.value = `${barEnd.x - barStart.x}px`
  yStart.value = `${barStart.y}px`
  yLength.value = `${barEnd.y - barStart.y}px`
  xFull.value = range.value.x === viewportSize.value.x
  yFull.value = range.value.y === viewportSize.value.y
})

const dragging = ref<Vec2>()

function dragEventsHandler(axis: 'x' | 'y') {
  return usePointer((pos, _event, eventType) => {
    switch (eventType) {
      case 'start': {
        const factor = Vec2.DotProduct(range.value, viewportSize.value.reciprocal())
        const speed = new Vec2(axis === 'x' ? factor.x : 0, axis === 'y' ? factor.y : 0)
        if (speed.isZero()) return
        dragging.value = speed
        emit('scroll', { type: 'start' })
        break
      }
      case 'move': {
        if (!dragging.value) return
        const startOffset = Vec2.DotProduct(pos.relative, dragging.value)
        emit('scroll', { type: 'move', startOffset })
        break
      }
      case 'stop': {
        if (!dragging.value) return
        dragging.value = undefined
        emit('scroll', { type: 'stop' })
        break
      }
    }
  })
}

const xDrag = dragEventsHandler('x')
const yDrag = dragEventsHandler('y')
</script>
<script lang="ts">
export type ScrollbarEvent =
  | {
      type: 'start'
    }
  | {
      type: 'move'
      startOffset: Vec2
    }
  | {
      type: 'stop'
    }
</script>

<template>
  <div ref="element" class="ScrollBar" @click.stop @pointerdown.stop @pointerup.stop>
    <div class="bar vertical" :class="{ full: yFull }" v-on="yDrag.events" />
    <div class="bar horizontal" :class="{ full: xFull }" v-on="xDrag.events" />
  </div>
</template>

<style scoped>
.ScrollBar {
  position: absolute;
  width: 100%;
  height: 100%;
  pointer-events: none;
}

.vertical {
  position: absolute;
  top: v-bind('yStart');
  height: v-bind('yLength');
  width: v-bind('`${BAR_WIDTH}px`');
  right: 2px;
  margin-top: v-bind('`${BAR_END_MARGIN}px`');
  margin-bottom: v-bind('`${BAR_WIDTH + BAR_END_MARGIN}px`');
}

.horizontal {
  position: absolute;
  left: v-bind('xStart');
  width: v-bind('xLength');
  height: v-bind('`${BAR_WIDTH}px`');
  bottom: 2px;
  margin-left: v-bind('`${BAR_END_MARGIN}px`');
  margin-right: v-bind('`${BAR_WIDTH + BAR_END_MARGIN}px`');
}

.bar {
  border-radius: v-bind('`${BAR_WIDTH / 2}px`');
  pointer-events: all;
  background-color: rgba(170 170 170 / 50%);
  transition: background-color 0.2s ease-in;
  &:hover {
    transition: background-color 0.2s ease-in;
    background-color: rgba(150 150 150 / 75%);
  }
  &:active {
    transition: none;
    background-color: rgba(130 130 130 / 100%);
  }
}

.full {
  transition: opacity 0.2s ease-in;
  opacity: 0;
  &:hover {
    transition: opacity 0.2s ease-in;
    opacity: 1;
  }
  &:active {
    transition: none;
    opacity: 1;
  }
}
</style>
