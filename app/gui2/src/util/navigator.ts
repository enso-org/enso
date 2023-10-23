import { PointerButtonMask, useEvent, usePointer, useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { computed, proxyRefs, ref, type Ref } from 'vue'

function elemRect(target: Element | undefined): Rect {
  if (target != null && target instanceof Element) {
    const domRect = target.getBoundingClientRect()
    return new Rect(new Vec2(domRect.x, domRect.y), new Vec2(domRect.width, domRect.height))
  }
  return Rect.Zero()
}

export type NavigatorComposable = ReturnType<typeof useNavigator>
export function useNavigator(viewportNode: Ref<Element | undefined>) {
  const size = useResizeObserver(viewportNode)
  const center = ref<Vec2>(Vec2.Zero())
  const scale = ref(1)
  const panPointer = usePointer((pos) => {
    center.value = center.value.addScaled(pos.delta, -1 / scale.value)
  }, PointerButtonMask.Auxiliary)

  function eventScreenPos(e: PointerEvent): Vec2 {
    return new Vec2(e.clientX, e.clientY)
  }

  function clientToScenePos(clientPos: Vec2): Vec2 {
    const rect = elemRect(viewportNode.value)
    const canvasPos = clientPos.sub(rect.pos)
    const v = viewport.value
    return new Vec2(
      v.pos.x + v.size.x * (canvasPos.x / rect.size.x),
      v.pos.y + v.size.y * (canvasPos.y / rect.size.y),
    )
  }

  let zoomPivot = Vec2.Zero()
  const zoomPointer = usePointer((pos, _event, ty) => {
    if (ty === 'start') {
      zoomPivot = clientToScenePos(pos.initial)
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

  const translate = computed<Vec2>(() => {
    const nodeSize = size.value
    const { x, y } = center.value
    const s = scale.value
    const w = nodeSize.x / s
    const h = nodeSize.y / s
    return new Vec2(-x + w / 2, -y + h / 2)
  })

  const transform = computed(
    () => `scale(${scale.value}) translate(${translate.value.x}px, ${translate.value.y}px)`,
  )

  const prescaledTransform = computed(
    () => `translate(${translate.value.x * scale.value}px, ${translate.value.y * scale.value}px)`,
  )

  useEvent(
    window,
    'contextmenu',
    (e) => {
      e.preventDefault()
    },
    { capture: true },
  )

  const eventMousePos = ref<Vec2 | null>(null)
  const sceneMousePos = computed(() =>
    eventMousePos.value ? clientToScenePos(eventMousePos.value) : null,
  )

  return proxyRefs({
    events: {
      pointermove(e: PointerEvent) {
        eventMousePos.value = eventScreenPos(e)
        panPointer.events.pointermove(e)
        zoomPointer.events.pointermove(e)
      },
      pointerleave() {
        eventMousePos.value = null
      },
      pointerup(e: PointerEvent) {
        panPointer.events.pointerup(e)
        zoomPointer.events.pointerup(e)
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
          const delta = new Vec2(e.deltaX, e.deltaY)
          center.value = center.value.addScaled(delta, 1 / scale.value)
        }
      },
    },
    translate,
    scale,
    viewBox,
    transform,
    /** Use this transform instead, if the element should not be scaled. */
    prescaledTransform,
    sceneMousePos,
  })
}
