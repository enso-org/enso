/** @file A Vue composable for panning and zooming a DOM element. */

import { useApproach } from '@/composables/animation'
import { PointerButtonMask, useEvent, usePointer, useResizeObserver } from '@/composables/events'
import type { KeyboardComposable } from '@/composables/keyboard'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, proxyRefs, shallowRef, type Ref } from 'vue'

type ScaleRange = readonly [number, number]
const WHEEL_SCALE_RANGE: ScaleRange = [0.5, 10]
const DRAG_SCALE_RANGE: ScaleRange = [0.1, 10]
const PAN_AND_ZOOM_DEFAULT_SCALE_RANGE: ScaleRange = [0.1, 1]
const ZOOM_STEP_DEFAULT_SCALE_RANGE: ScaleRange = [0.1, 10]

function elemRect(target: Element | undefined): Rect {
  if (target != null && target instanceof Element)
    return Rect.FromDomRect(target.getBoundingClientRect())
  return Rect.Zero
}

export type NavigatorComposable = ReturnType<typeof useNavigator>
export function useNavigator(viewportNode: Ref<Element | undefined>, keyboard: KeyboardComposable) {
  const size = useResizeObserver(viewportNode)
  const targetCenter = shallowRef<Vec2>(Vec2.Zero)
  const targetX = computed(() => targetCenter.value.x)
  const targetY = computed(() => targetCenter.value.y)
  const centerX = useApproach(targetX)
  const centerY = useApproach(targetY)
  const center = computed({
    get() {
      return new Vec2(centerX.value, centerY.value)
    },
    set(value) {
      targetCenter.value = value
      centerX.value = value.x
      centerY.value = value.y
    },
  })
  const targetScale = shallowRef(1)
  const animatedScale = useApproach(targetScale)
  const scale = computed({
    get() {
      return animatedScale.value
    },
    set(value) {
      targetScale.value = value
      animatedScale.value = value
    },
  })
  const panPointer = usePointer((pos) => {
    center.value = center.value.addScaled(pos.delta, -1 / scale.value)
  }, PointerButtonMask.Auxiliary)

  function eventScreenPos(e: { clientX: number; clientY: number }): Vec2 {
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

  function clientToSceneRect(clientRect: Rect): Rect {
    const rect = elemRect(viewportNode.value)
    const canvasPos = clientRect.pos.sub(rect.pos)
    const v = viewport.value
    const pos = new Vec2(
      v.pos.x + v.size.x * (canvasPos.x / rect.size.x),
      v.pos.y + v.size.y * (canvasPos.y / rect.size.y),
    )
    const size = new Vec2(
      v.size.x * (clientRect.size.x / rect.size.x),
      v.size.y * (clientRect.size.y / rect.size.y),
    )
    return new Rect(pos, size)
  }

  function panAndZoomTo(
    rect: Rect,
    minScale = PAN_AND_ZOOM_DEFAULT_SCALE_RANGE[0],
    maxScale = PAN_AND_ZOOM_DEFAULT_SCALE_RANGE[1],
  ) {
    if (!viewportNode.value) return
    targetScale.value = Math.max(
      minScale,
      Math.min(
        maxScale,
        viewportNode.value.clientHeight / rect.height,
        viewportNode.value.clientWidth / rect.width,
      ),
    )
    const centerX =
      !Number.isFinite(rect.left) && !Number.isFinite(rect.width) ? 0 : rect.left + rect.width / 2
    const centerY =
      !Number.isFinite(rect.top) && !Number.isFinite(rect.height) ? 0 : rect.top + rect.height / 2
    targetCenter.value = new Vec2(centerX, centerY)
  }

  /** Pan to include the given prioritized list of coordinates.
   *
   *  The view will be offset to include each coordinate, unless the coordinate cannot be fit in the viewport without
   *  losing a previous (higher-priority) coordinate; in that case, shift the viewport as close as possible to the
   *  coordinate while still satisfying the more important constraints.
   *
   *  If all provided constraints can be met, the viewport will be moved the shortest distance that fits all the
   *  coordinates in view.
   */
  function panTo(points: Partial<Vec2>[]) {
    let target = viewport.value
    for (const point of points.reverse()) target = target.offsetToInclude(point) ?? target
    targetCenter.value = target.center()
  }

  /** Pan immediately to center the viewport at the given point, in scene coordinates. */
  function scrollTo(newCenter: Vec2) {
    center.value = newCenter
  }

  let zoomPivot = Vec2.Zero
  const zoomPointer = usePointer((pos, _event, ty) => {
    if (ty === 'start') {
      zoomPivot = clientToScenePos(pos.initial)
    }

    const prevScale = scale.value
    updateScale((oldValue) => oldValue * Math.exp(-pos.delta.y / 100), DRAG_SCALE_RANGE)
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

  let isPointerDown = false
  let scrolledThisFrame = false
  const eventMousePos = shallowRef<Vec2 | null>(null)
  let eventTargetScrollPos: Vec2 | null = null
  const sceneMousePos = computed(() =>
    eventMousePos.value ? clientToScenePos(eventMousePos.value) : null,
  )

  useEvent(
    window,
    'scroll',
    (e) => {
      if (
        !isPointerDown ||
        scrolledThisFrame ||
        !eventMousePos.value ||
        !(e.target instanceof Element)
      )
        return
      scrolledThisFrame = true
      requestAnimationFrame(() => (scrolledThisFrame = false))
      if (!(e.target instanceof Element)) return
      const newScrollPos = new Vec2(e.target.scrollLeft, e.target.scrollTop)
      if (eventTargetScrollPos !== null) {
        const delta = newScrollPos.sub(eventTargetScrollPos)
        const mouseDelta = new Vec2(
          (delta.x * e.target.clientWidth) / e.target.scrollWidth,
          (delta.y * e.target.clientHeight) / e.target.scrollHeight,
        )
        eventMousePos.value = eventMousePos.value?.add(mouseDelta) ?? null
      }
      eventTargetScrollPos = newScrollPos
    },
    { capture: true },
  )

  useEvent(
    window,
    'scrollend',
    () => {
      eventTargetScrollPos = null
    },
    { capture: true },
  )

  /** Clamp the value to the given bounds, except if it is already outside the bounds allow the new value to be less
   *  outside the bounds. */
  function directedClamp(oldValue: number, newValue: number, [min, max]: ScaleRange): number {
    if (newValue > oldValue) return Math.min(max, newValue)
    else return Math.max(min, newValue)
  }

  function updateScale(f: (value: number) => number, range: ScaleRange) {
    const oldValue = scale.value
    scale.value = directedClamp(oldValue, f(oldValue), range)
  }

  function zoomStepToScaleFactor(step: number): number {
    return Math.pow(2, step / 2)
  }
  function zoomStepFromScaleFactor(scale: number): number {
    return Math.round(Math.log2(scale) * 2)
  }

  function stepZoom(zoomStepDelta: number, range: ScaleRange = ZOOM_STEP_DEFAULT_SCALE_RANGE) {
    updateScale(
      (oldValue) => zoomStepToScaleFactor(zoomStepFromScaleFactor(oldValue) + zoomStepDelta),
      range,
    )
  }

  return proxyRefs({
    events: {
      dragover(e: DragEvent) {
        eventMousePos.value = eventScreenPos(e)
      },
      dragleave() {
        eventMousePos.value = null
      },
      pointermove(e: PointerEvent) {
        eventMousePos.value = eventScreenPos(e)
        panPointer.events.pointermove(e)
        zoomPointer.events.pointermove(e)
      },
      pointerleave() {
        eventMousePos.value = null
      },
      pointerup(e: PointerEvent) {
        isPointerDown = false
        panPointer.events.pointerup(e)
        zoomPointer.events.pointerup(e)
      },
      pointerdown(e: PointerEvent) {
        isPointerDown = true
        panPointer.events.pointerdown(e)
        zoomPointer.events.pointerdown(e)
      },
      wheel(e: WheelEvent) {
        e.preventDefault()
        if (e.ctrlKey) {
          // A pinch gesture is represented by setting `e.ctrlKey`. It can be distinguished from an actual Ctrl+wheel
          // combination because the real Ctrl key emits keyup/keydown events.
          const isGesture = !keyboard.ctrl
          if (isGesture) {
            // OS X trackpad events provide usable rate-of-change information.
            updateScale(
              (oldValue: number) => oldValue * Math.exp(-e.deltaY / 100),
              WHEEL_SCALE_RANGE,
            )
          } else {
            // Mouse wheel rate information is unreliable. We just step in the direction of the sign.
            stepZoom(-Math.sign(e.deltaY), WHEEL_SCALE_RANGE)
          }
        } else {
          const delta = new Vec2(e.deltaX, e.deltaY)
          center.value = center.value.addScaled(delta, 1 / scale.value)
        }
      },
      contextmenu(e: Event) {
        e.preventDefault()
      },
    },
    translate,
    targetScale,
    scale,
    viewBox,
    transform,
    /** Use this transform instead, if the element should not be scaled. */
    prescaledTransform,
    sceneMousePos,
    clientToScenePos,
    clientToSceneRect,
    panAndZoomTo,
    panTo,
    viewport,
    stepZoom,
    scrollTo,
  })
}
