/** @file A Vue composable for panning and zooming a DOM element. */

import { useApproach, useApproachVec } from '@/composables/animation'
import {
  PointerButtonMask,
  useArrows,
  useEvent,
  usePointer,
  useResizeObserver,
  useWheelActions,
} from '@/composables/events'
import type { KeyboardComposable } from '@/composables/keyboard'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, proxyRefs, readonly, shallowRef, toRef, watch, type Ref } from 'vue'

type ScaleRange = readonly [number, number]
const PAN_AND_ZOOM_DEFAULT_SCALE_RANGE: ScaleRange = [0.1, 1]
const ZOOM_LEVELS = [
  0.1, 0.25, 0.33, 0.5, 0.67, 0.75, 0.8, 0.9, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0, 5.0,
]
const DEFAULT_SCALE_RANGE: ScaleRange = [Math.min(...ZOOM_LEVELS), Math.max(...ZOOM_LEVELS)]
const ZOOM_LEVELS_REVERSED = [...ZOOM_LEVELS].reverse()
/** The fraction of the next zoom level.
 * If we are that close to next zoom level, we should choose the next one instead
 * to avoid small unnoticeable changes to zoom. */
const ZOOM_SKIP_THRESHOLD = 0.05
const WHEEL_CAPTURE_DURATION_MS = 250

function elemRect(target: Element | undefined): Rect {
  if (target != null && target instanceof Element)
    return Rect.FromDomRect(target.getBoundingClientRect())
  return Rect.Zero
}

export interface NavigatorOptions {
  /* A predicate deciding if given event should initialize navigation */
  predicate?: (e: PointerEvent | KeyboardEvent) => boolean
}

export type NavigatorComposable = ReturnType<typeof useNavigator>
export function useNavigator(
  viewportNode: Ref<Element | undefined>,
  keyboard: KeyboardComposable,
  options: NavigatorOptions = {},
) {
  const predicate = options.predicate ?? ((_) => true)
  const size = useResizeObserver(viewportNode)
  const targetCenter = shallowRef<Vec2>(Vec2.Zero)
  const center = useApproachVec(targetCenter, 100, 0.02)

  const viewportRect = shallowRef<Rect>(Rect.Zero)
  function updateViewportRect() {
    viewportRect.value = elemRect(viewportNode.value)
  }

  watch(size, updateViewportRect, { immediate: true })

  const targetScale = shallowRef(1)
  const scale = useApproach(targetScale)
  const panPointer = usePointer(
    (pos) => scrollTo(center.value.addScaled(pos.delta, -1 / scale.value)),
    {
      requiredButtonMask: PointerButtonMask.Auxiliary,
      predicate: (e) => e.target === e.currentTarget && predicate(e),
    },
  )

  const panArrows = useArrows(
    (pos) => scrollTo(center.value.addScaled(pos.delta, 1 / scale.value)),
    { predicate, velocity: 1000 },
  )

  function eventScreenPos(e: { clientX: number; clientY: number }): Vec2 {
    return new Vec2(e.clientX, e.clientY)
  }

  function clientToScenePos(clientPos: Vec2): Vec2 {
    const rect = viewportRect.value
    const canvasPos = clientPos.sub(rect.pos)
    const v = viewport.value
    return new Vec2(
      v.pos.x + v.size.x * (canvasPos.x / rect.size.x),
      v.pos.y + v.size.y * (canvasPos.y / rect.size.y),
    )
  }

  function clientToSceneRect(clientRect: Rect): Rect {
    const rect = viewportRect.value
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
    skipAnimation = false,
  ) {
    resetTargetFollowing()
    if (!viewportNode.value) return
    targetScale.value = Math.max(
      minScale,
      Math.min(
        maxScale,
        viewportNode.value.clientHeight / rect.height,
        viewportNode.value.clientWidth / rect.width,
      ),
    )
    targetCenter.value = rect.center().finiteOrZero()
    if (skipAnimation) {
      scale.skip()
      center.skip()
    }
  }

  let targetPoints: Partial<Vec2>[] = []

  function resetTargetFollowing() {
    targetPoints = []
  }

  watch(size, () => {
    if (targetPoints.length > 0) {
      panToImpl(targetPoints)
    }
  })

  /** As `panTo`, but also follow the points if the viewport size is changing.
   *
   * The following is working until manual panning by user input or until the next call to any `pan…` function.
   */
  function panToThenFollow(points: Partial<Vec2>[]) {
    targetPoints = points
    panToImpl(points)
  }

  /** Pan to include the given prioritized list of coordinates.
   *
   *  The view will be offset to include each coordinate, unless the coordinate cannot be fit in the viewport without
   *  losing a previous (higher-priority) coordinate; in that case, shift the viewport as close as possible to the
   *  coordinate while still satisfying the more important constraints.
   *
   *  If all provided constraints can be met, the viewport will be moved the shortest distance that fits all the
   *  coordinates in view.
   *
   *  If the viewport size is changing, the view will be panned to preserve the coordinates in view until a new call
   *  to `panToThenFollow` or similar functions (like `panAndZoomTo`), or until a manual pan by user input.
   */
  function panTo(points: Partial<Vec2>[]) {
    resetTargetFollowing()
    panToImpl(points)
  }

  function panToImpl(points: Partial<Vec2>[]) {
    let target = viewport.value
    for (const point of points.reverse()) target = target.offsetToInclude(point) ?? target
    targetCenter.value = target.center()
  }

  /** Pan immediately to center the viewport at the given point, in scene coordinates. */
  function scrollTo(newCenter: Vec2) {
    resetTargetFollowing()
    targetCenter.value = newCenter
    center.skip()
  }

  /** Set viewport center point and scale value immediately, skipping animations. */
  function setCenterAndScale(newCenter: Vec2, newScale: number) {
    resetTargetFollowing()
    targetCenter.value = newCenter
    targetScale.value = newScale
    scale.skip()
    center.skip()
  }

  let zoomPivot = Vec2.Zero
  const zoomPointer = usePointer(
    (pos, _event, ty) => {
      if (ty === 'start') {
        zoomPivot = clientToScenePos(pos.initial)
      }

      const prevScale = scale.value
      updateScale((oldValue) => oldValue * Math.exp(-pos.delta.y / 100))
      scrollTo(
        center.value
          .sub(zoomPivot)
          .scale(prevScale / scale.value)
          .add(zoomPivot),
      )
    },
    {
      requiredButtonMask: PointerButtonMask.Secondary,
      predicate: (e) => e.target === e.currentTarget && predicate(e),
    },
  )

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

  function updateScale(f: (value: number) => number, range: ScaleRange = DEFAULT_SCALE_RANGE) {
    const oldValue = scale.value
    targetScale.value = directedClamp(oldValue, f(oldValue), range)
    scale.skip()
  }

  /** Step to the next level from {@link ZOOM_LEVELS}.
   * @param zoomStepDelta step direction. If positive select larger zoom level; if negative  select smaller.
   * If 0, resets zoom level to 1.0. */
  function stepZoom(zoomStepDelta: number) {
    const oldValue = targetScale.value
    const insideThreshold = (level: number) =>
      Math.abs(oldValue - level) <= level * ZOOM_SKIP_THRESHOLD
    if (zoomStepDelta > 0) {
      const lastZoomLevel = ZOOM_LEVELS[ZOOM_LEVELS.length - 1]!
      targetScale.value =
        ZOOM_LEVELS.find((level) => level > oldValue && !insideThreshold(level)) ?? lastZoomLevel
    } else if (zoomStepDelta < 0) {
      const firstZoomLevel = ZOOM_LEVELS[0]!
      targetScale.value =
        ZOOM_LEVELS_REVERSED.find((level) => level < oldValue && !insideThreshold(level)) ??
        firstZoomLevel
    } else {
      targetScale.value = 1.0
    }
    scale.skip()
  }

  /**
   * Translate the viewport as necessary to ensure that a particular client point corresponds to the same scene point
   * before and after running the provided function.
   */
  function maintainingScenePosAtClientPoint<T>(clientPos: Vec2, f: () => T): T {
    resetTargetFollowing()
    const scenePos0 = clientToScenePos(clientPos)
    const result = f()
    const scenePos1 = clientToScenePos(clientPos)
    targetCenter.value = center.value.add(scenePos0.sub(scenePos1))
    center.skip()
    return result
  }

  const { events: wheelEvents, captureEvents: wheelEventsCapture } = useWheelActions(
    keyboard,
    WHEEL_CAPTURE_DURATION_MS,
    (e, inputType) => {
      maintainingScenePosAtClientPoint(eventScreenPos(e), () => {
        if (inputType === 'trackpad') {
          // OS X trackpad events provide usable rate-of-change information.
          updateScale((oldValue: number) => oldValue * Math.exp(-e.deltaY / 100))
        } else {
          // Mouse wheel rate information is unreliable. We just step in the direction of the sign.
          stepZoom(-Math.sign(e.deltaY))
        }
      })
    },
    (e) => {
      const delta = new Vec2(e.deltaX, e.deltaY)
      scrollTo(center.value.addScaled(delta, 1 / scale.value))
    },
  )

  return proxyRefs({
    pointerEvents: {
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
      contextmenu(e: Event) {
        e.preventDefault()
      },
      wheel: wheelEvents.wheel,
    },
    pointerEventsCapture: wheelEventsCapture,
    keyboardEvents: panArrows.events,
    translate: readonly(translate),
    targetCenter: readonly(targetCenter),
    targetScale: readonly(targetScale),
    scale: readonly(toRef(scale, 'value')),
    viewBox: readonly(viewBox),
    transform: readonly(transform),
    /** Use this transform instead, if the element should not be scaled. */
    prescaledTransform,
    sceneMousePos,
    clientToScenePos,
    clientToSceneRect,
    panAndZoomTo,
    panTo,
    panToThenFollow,
    viewport,
    stepZoom,
    scrollTo,
    setCenterAndScale,
  })
}
