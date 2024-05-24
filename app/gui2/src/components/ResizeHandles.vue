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
function resizeHandler(resizeX: 'left' | 'right' | false, resizeY: boolean) {
  const resizing = {
    left: resizeX === 'left',
    right: resizeX === 'right',
    bottom: resizeY,
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
    }
  })
}

const handler = {
  left: resizeHandler('left', false).events,
  right: resizeHandler('right', false).events,
  bottom: resizeHandler(false, true).events,
  bottomLeft: resizeHandler('left', true).events,
  bottomRight: resizeHandler('right', true).events,
}
</script>

<template>
  <div v-if="props.left" class="left" v-on="handler.left" />
  <div v-if="props.right" class="right" v-on="handler.right" />
  <div v-if="props.bottom" class="bottom" v-on="handler.bottom" />
  <div v-if="props.bottom && props.left" class="bottom left" v-on="handler.bottomLeft" />
  <div v-if="props.bottom && props.right" class="bottom right" v-on="handler.bottomRight" />
</template>

<style scoped>
.left {
  position: absolute;
  cursor: ew-resize;
  top: 0;
  height: 100%;
}
.right {
  position: absolute;
  cursor: ew-resize;
  top: 0;
  height: 100%;
}
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
  left: calc(100% - var(--resize-handle-inside));
  width: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}
.bottom {
  top: calc(100% - var(--resize-handle-inside));
  height: calc(var(--resize-handle-inside) + var(--resize-handle-outside));
}

.bottom.right {
  cursor: nwse-resize;
}
.bottom.left {
  cursor: nesw-resize;
}
</style>
