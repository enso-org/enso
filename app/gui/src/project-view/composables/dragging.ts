import { useDoubleClick } from '@/composables/doubleClick'
import { usePointer } from '@/composables/events'
import type { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'

const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50

export interface ClickableDraggableOptions {
  dragMove?: (pos: Vec2) => void
  dragCommit?: () => void
  dragCancel?: () => void
  click?: (e: PointerEvent) => void
  doubleClick?: (e: PointerEvent) => void
}

/**
 * Composable which distinguishes clicking and dragging events, to support an element that allows
 * either.
 */
export function useClickableDraggable(on: ClickableDraggableOptions) {
  const startEpochMs = ref(0)
  const significantMove = ref(false)

  const click = useDoubleClick(
    (e: PointerEvent) => {
      if (!significantMove.value) on?.click?.(e)
    },
    (e: PointerEvent) => {
      if (!significantMove.value) on?.doubleClick?.(e)
    },
  ).handleClick

  const dragPointer = usePointer(
    (pos, event, type) => {
      if (type !== 'start') {
        if (
          !significantMove.value &&
          (Number(new Date()) - startEpochMs.value >= MAXIMUM_CLICK_LENGTH_MS ||
            pos.relative.lengthSquared() >= MAXIMUM_CLICK_DISTANCE_SQ)
        ) {
          // If this is clearly a drag (not a click), the node itself capture pointer events to
          // prevent `click` on widgets.
          if (event.currentTarget instanceof Element)
            event.currentTarget.setPointerCapture?.(event.pointerId)
          significantMove.value = true
        }
        on.dragMove?.(pos.relative)
      }
      switch (type) {
        case 'start':
          startEpochMs.value = Number(new Date())
          significantMove.value = false
          break
        case 'stop':
          startEpochMs.value = 0
          on.dragCommit?.()
          break
        case 'cancel':
          startEpochMs.value = 0
          on.dragCancel?.()
          break
      }
    },
    // Pointer is captured by `target`, to make it receive the `up` and `click` event in case this
    // is not going to be a node drag.
    { pointerCapturedBy: 'target' },
  )
  const isDragged = computed(() => dragPointer.dragging && significantMove.value)
  return { isDragged, pointerEvents: { ...dragPointer.events, click } }
}
