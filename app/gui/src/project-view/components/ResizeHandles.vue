<script setup lang="ts">
import { usePointer } from '@/composables/events'
import { selectFields } from '@/util/data/object'
import { Rect, type BoundsSet } from '@/util/data/rect'

const bounds = defineModel<Rect>({ required: true })
const props = defineProps<BoundsSet>()
const emit = defineEmits<{
  'update:resizing': [BoundsSet]
}>()

let initialBounds: Rect | undefined = undefined
function resizeHandler(resizeX: 'left' | 'right' | false, resizeY: 'top' | 'bottom' | false) {
  const resizing = {
    left: resizeX === 'left',
    right: resizeX === 'right',
    top: resizeY === 'top',
    bottom: resizeY === 'bottom',
  }
  return usePointer((pos, _, type) => {
    switch (type) {
      case 'start':
        initialBounds = bounds.value
        emit('update:resizing', resizing)
        break
      case 'move':
        if (!initialBounds) break
        bounds.value = initialBounds.withBoundsClamped(
          selectFields(resizing, {
            top: initialBounds.top + pos.relative.y,
            bottom: initialBounds.bottom + pos.relative.y,
            left: initialBounds.left + pos.relative.x,
            right: initialBounds.right + pos.relative.x,
          }),
        )
        break
      case 'stop':
        emit('update:resizing', {})
        break
      case 'cancel':
        if (initialBounds) bounds.value = initialBounds
        emit('update:resizing', {})
        break
    }
  })
}

const handler = {
  left: resizeHandler('left', false).events,
  right: resizeHandler('right', false).events,
  top: resizeHandler(false, 'top').events,
  bottom: resizeHandler(false, 'bottom').events,
  bottomLeft: resizeHandler('left', 'bottom').events,
  bottomRight: resizeHandler('right', 'bottom').events,
}
</script>

<template>
  <div v-if="props.left" class="left" v-on="handler.left" />
  <div v-if="props.right" class="right" v-on="handler.right" />
  <div v-if="props.top" class="top" v-on="handler.top" />
  <div v-if="props.bottom" class="bottom" v-on="handler.bottom" />
  <svg v-if="props.bottom && props.left" class="corner bottom left" v-on="handler.bottomLeft">
    <circle />
  </svg>
  <svg v-if="props.bottom && props.right" class="corner bottom right" v-on="handler.bottomRight">
    <circle />
  </svg>
</template>

<style scoped>
.left,
.right {
  position: absolute;
  cursor: ew-resize;
  top: 0;
  height: 100%;
}
.top,
.bottom {
  position: absolute;
  cursor: ns-resize;
  width: 100%;
}

.left {
  left: calc(0px - var(--resize-handle-outside));
  width: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}
.right {
  right: calc(0px - var(--resize-handle-outside));
  width: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}
.top {
  top: calc(0px - var(--resize-handle-inside));
  height: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}
.bottom {
  top: unset;
  bottom: calc(0px - var(--resize-handle-outside));
  height: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}

.corner {
  pointer-events: none;
  z-index: 2;
  --corner-size: calc(
    max(var(--resize-handle-inside), var(--resize-handle-radius, 0)) + var(--resize-handle-outside)
  );
  width: var(--corner-size);
  height: var(--corner-size);
  & circle {
    pointer-events: all;
    r: calc(
      var(--resize-handle-radius, 0) + (var(--resize-handle-outside) - var(--resize-handle-inside)) /
        2
    );
    stroke: transparent;
    stroke-width: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
    fill: none;
  }
}

.bottom.right {
  cursor: nwse-resize;
}
.bottom.left {
  cursor: nesw-resize;
  & circle {
    cx: var(--corner-size);
  }
}

.left,
.right,
.top,
.bottom {
  z-index: 1;
}
</style>
