import { useCallbackRegistry, type RegisterCallback } from '$/utils/data/callbacks'
import type { ToValue } from '$/utils/reactivity'
import { selectFields } from '@/util/data/object'
import { Rect, type BoundsSet } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { toValue } from 'vue'

interface ResizeHandlesOptions {
  position?: ToValue<Vec2>
  size: ToValue<Vec2>
  scale?: ToValue<number>
}

export interface ResizeHandlesEventRegistry {
  onMove: RegisterCallback<Vec2>
  onResizingChange: RegisterCallback<BoundsSet>
  onResize: RegisterCallback<Vec2>
  onResizeWidth: (callback: (value: number, delta: number) => void) => void
  onResizeHeight: RegisterCallback<number>
}

/** Provides a higher-level API on top of the {@link ResizeHandles} component. */
export function useResizeHandles(options: ResizeHandlesOptions) {
  let resizing: BoundsSet = {}
  let initialBounds: Rect | undefined = undefined
  const onMove = useCallbackRegistry<[Vec2]>()
  const onResizingChange = useCallbackRegistry<[BoundsSet]>()
  const onResize = useCallbackRegistry<[Vec2]>()
  const onResizeWidth = useCallbackRegistry<[number, number]>()
  const onResizeHeight = useCallbackRegistry<[number]>()
  const events = {
    'update:resizing': (value: BoundsSet) => {
      resizing = value
      initialBounds = new Rect(toValue(options.position) ?? Vec2.Zero, toValue(options.size))
      onResizingChange.run(resizing)
    },
    'update:movement': (delta: Vec2) => {
      const scaleValue = toValue(options.scale)
      const scaled = scaleValue ? delta.scale(1 / scaleValue) : delta
      if (!initialBounds) return
      const bounds = initialBounds.withBoundsClamped(
        selectFields(resizing, {
          top: initialBounds.top + scaled.y,
          bottom: initialBounds.bottom + scaled.y,
          left: initialBounds.left + scaled.x,
          right: initialBounds.right + scaled.x,
        }),
      )
      onResize.run(bounds.size)
      if (resizing.left || resizing.right) onResizeWidth.run(bounds.size.x, delta.x)
      if (resizing.top || resizing.bottom) onResizeHeight.run(bounds.size.y)
      if (options.position) {
        if (resizing.top || resizing.left) onMove.run(bounds.pos)
      }
    },
  }
  const registry: ResizeHandlesEventRegistry = {
    onMove: onMove.register,
    onResizingChange: onResizingChange.register,
    onResize: onResize.register,
    onResizeWidth: onResizeWidth.register,
    onResizeHeight: onResizeHeight.register,
  }
  return {
    events,
    ...registry,
  }
}
