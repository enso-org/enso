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
const TRACK_WIDTH = BAR_WIDTH + 2

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
  const viewportFraction = Vec2.ElementwiseProduct(viewportSize.value, range.value.reciprocal())
  const barStartFraction = Vec2.ElementwiseProduct(props.position, range.value.reciprocal())
  const trackLength = viewportSize.value.sub(
    new Vec2(BAR_END_MARGIN * 2 + TRACK_WIDTH, BAR_END_MARGIN * 2 + TRACK_WIDTH),
  )
  const barStart = Vec2.ElementwiseProduct(trackLength, barStartFraction).max(Vec2.Zero)
  const barLength = Vec2.ElementwiseProduct(trackLength, viewportFraction)
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
        const factor = Vec2.ElementwiseProduct(range.value, viewportSize.value.reciprocal())
        const speed = new Vec2(axis === 'x' ? factor.x : 0, axis === 'y' ? factor.y : 0)
        if (speed.isZero()) return
        dragging.value = speed
        emit('scroll', { type: 'start' })
        break
      }
      case 'move': {
        if (!dragging.value) return
        const startOffset = Vec2.ElementwiseProduct(pos.relative, dragging.value)
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
    return true
  })
}

function handleTrackClick(axis: 'x' | 'y', event: MouseEvent) {
  if (!element.value) return
  const bounds = element.value.getBoundingClientRect()
  const proportionalPos =
    Vec2.FromXY(event).sub(Vec2.FromXY(bounds)).getAxis(axis) / Vec2.FromSize(bounds).getAxis(axis)
  const position = proportionalPos * props.size.getAxis(axis)
  emit('scroll', { type: 'jump', axis, position })
}

const dragSlider = {
  x: dragEventsHandler('x').events,
  y: dragEventsHandler('y').events,
}
const clickTrack = {
  x: (event: MouseEvent) => handleTrackClick('x', event),
  y: (event: MouseEvent) => handleTrackClick('y', event),
}
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
  | {
      type: 'jump'
      axis: 'x' | 'y'
      position: number
    }
</script>

<template>
  <div ref="element" class="ScrollBar">
    <div class="track vertical" @pointerdown.stop="clickTrack.y">
      <div class="bar vertical" v-on.stop="dragSlider.y" />
    </div>
    <div class="track horizontal" @pointerdown.stop="clickTrack.x">
      <div class="bar horizontal" v-on.stop="dragSlider.x" />
    </div>
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
  width: v-bind('`${TRACK_WIDTH}px`');
  height: 100%;
  right: 1px;
  margin-top: v-bind('`${BAR_END_MARGIN}px`');
  margin-bottom: v-bind('`${TRACK_WIDTH + BAR_END_MARGIN}px`');
}
.bar.vertical {
  left: 1px;
  top: v-bind('yStart');
  height: v-bind('yLength');
}

.horizontal {
  position: absolute;
  height: v-bind('`${TRACK_WIDTH}px`');
  width: 100%;
  bottom: 1px;
  margin-left: v-bind('`${BAR_END_MARGIN}px`');
  margin-right: v-bind('`${TRACK_WIDTH + BAR_END_MARGIN}px`');
}
.bar.horizontal {
  top: 1px;
  left: v-bind('xStart');
  width: v-bind('xLength');
}

.bar {
  border-radius: 100px;
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

.track {
  pointer-events: all;
  &::before {
    content: '';
    height: 100%;
  }
  background-color: rgba(150 150 150 / 15%);
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
