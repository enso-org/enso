import { computed, nextTick, proxyRefs, ref, type Ref } from 'vue'
import { PointerButtonMask, usePointer, useResizeObserver, useWindowEvent } from './events'
import { Vec2 } from './vec2'
import { Rect } from '@/stores/rect'

function elemRect(target: Element | undefined): Rect {
  if (target != null && target instanceof Element) {
    let domRect = target.getBoundingClientRect()
    return new Rect(new Vec2(domRect.x, domRect.y), new Vec2(domRect.width, domRect.height))
  }
  return Rect.Zero()
}

export function useNavigator(viewportNode: Ref<Element | undefined>) {
  const size = useResizeObserver(viewportNode)
  const center = ref<Vec2>(Vec2.Zero())
  const scale = ref(1)
  const panPointer = usePointer((pos) => {
    center.value = center.value.addScaled(pos.delta, -1 / scale.value)
  }, PointerButtonMask.Auxiliary)

  function eventToScenePos(event: PointerEvent, client?: Vec2): Vec2 {
    const rect = elemRect(viewportNode.value)
    const clientPos = client ?? new Vec2(event.clientX, event.clientY)
    const canvasPos = clientPos.sub(rect.pos)
    const v = viewport.value
    return new Vec2(
      v.pos.x + v.size.x * (canvasPos.x / rect.size.x),
      v.pos.y + v.size.y * (canvasPos.y / rect.size.y),
    )
  }

  let lastDragTimestamp: number = 0
  let dragHasMoved: boolean = false
  let zoomPivot = Vec2.Zero()
  const zoomPointer = usePointer((pos, event, ty) => {
    if (ty === 'start') {
      dragHasMoved = false
      zoomPivot = eventToScenePos(event, pos.initial)
    }
    if (ty === 'move') {
      dragHasMoved = true
    }
    if (dragHasMoved) {
      lastDragTimestamp = event.timeStamp
    }
    const prevScale = scale.value
    scale.value = Math.max(0.1, Math.min(10, scale.value * Math.exp(-pos.delta.y / 100)))
    center.value = center.value
      .sub(zoomPivot)
      .scale(prevScale / scale.value)
      .add(zoomPivot)
  }, PointerButtonMask.Secondary)

  const viewport = computed(() => {
    const nodeSize = size.value
    const { x, y } = center.value
    const s = scale.value
    const w = nodeSize.x / s
    const h = nodeSize.y / s
    return new Rect(new Vec2(x - w / 2, y - h / 2), new Vec2(w, h))
  })

  const viewBox = computed(() => {
    const v = viewport.value
    return `${v.pos.x} ${v.pos.y} ${v.size.x} ${v.size.y}`
  })

  const transform = computed(() => {
    const nodeSize = size.value
    const { x, y } = center.value
    const s = scale.value
    const w = nodeSize.x / s
    const h = nodeSize.y / s
    return `scale(${s}) translate(${-x + w / 2}px, ${-y + h / 2}px)`
  })

  useWindowEvent(
    'contextmenu',
    (e) => {
      if (lastDragTimestamp >= e.timeStamp) {
        e.preventDefault()
      }
    },
    { capture: true },
  )

  const sceneMousePos = ref<Vec2 | null>(null)

  return proxyRefs({
    events: {
      pointermove(e: PointerEvent) {
        sceneMousePos.value = eventToScenePos(e)
      },
      pointerleave() {
        sceneMousePos.value = null
      },
      pointerdown(e: PointerEvent) {
        panPointer.events.pointerdown(e)
        zoomPointer.events.pointerdown(e)
      },
      wheel(e: WheelEvent) {
        e.preventDefault()
        // When using a macbook trackpad, e.ctrlKey indicates a pinch gesture.
        if (e.ctrlKey) {
          const s = Math.exp(-e.deltaY / 100)
          scale.value = Math.min(Math.max(0.5, scale.value * s), 10)
        } else {
          let delta = new Vec2(e.deltaX, e.deltaY)
          center.value = center.value.addScaled(delta, 1 / scale.value)
        }
      },
    },
    scale,
    viewBox,
    transform,
    sceneMousePos,
  })
}
