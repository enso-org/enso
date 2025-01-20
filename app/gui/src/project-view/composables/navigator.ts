/** @file A Vue composable for panning and zooming a DOM element. */

import { useApproach, useApproachVec } from '@/composables/animation'
import {
  PointerButtonMask,
  useArrows,
  useResizeObserver,
  useWheelActions,
} from '@/composables/events'
import type { KeyboardComposable } from '@/composables/keyboard'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { useEventListener } from '@vueuse/core'
import { useGesture, type Handler } from '@vueuse/gesture'
import {
  computed,
  onScopeDispose,
  proxyRefs,
  readonly,
  ref,
  shallowRef,
  toRef,
  watch,
  type Ref,
} from 'vue'

type ScaleRange = readonly [number, number]
const PAN_AND_ZOOM_DEFAULT_SCALE_RANGE: ScaleRange = [0.1, 1]
const ZOOM_LEVELS = [
  0.1, 0.25, 0.33, 0.5, 0.67, 0.75, 0.8, 0.9, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0, 5.0,
]
const DEFAULT_SCALE_RANGE: ScaleRange = [Math.min(...ZOOM_LEVELS), Math.max(...ZOOM_LEVELS)]
const ZOOM_LEVELS_REVERSED = [...ZOOM_LEVELS].reverse()
/**
 * The fraction of the next zoom level.
 * If we are that close to next zoom level, we should choose the next one instead
 * to avoid small unnoticeable changes to zoom.
 */
const ZOOM_SKIP_THRESHOLD = 0.05
const WHEEL_CAPTURE_DURATION_MS = 250
const LONGPRESS_TIMEOUT = 500
const LONGPRESS_MAX_SLIDE = 10

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
/** TODO: Add docs */
export function useNavigator(
  viewportNode: Ref<HTMLElement | undefined>,
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

  const dragPredicate = (e: PointerEvent) => e.target === e.currentTarget && predicate(e)

  function eventIsTouch(e: Event) {
    return e.type.startsWith('touch') || (e instanceof PointerEvent && e.pointerType === 'touch')
  }

  const holdDragListeners: Array<Handler<'drag'>> = []
  function callHoldDragListeners(...params: Parameters<Handler<'drag'>>) {
    holdDragListeners.forEach((f) => f(...params))
  }

  let gesturePivot = Vec2.Zero
  let pinchScaleRatio = 1
  let lastPinchOrigin = Vec2.Zero
  let holdDragStarted = false

  type DragState = Parameters<Handler<'drag', PointerEvent>>[0]

  function handleDragPanning(state: DragState) {
    if (Vec2.FromTuple(state.movement).length() > LONGPRESS_MAX_SLIDE) cancelLongpress()
    scrollTo(center.value.addScaled(Vec2.FromTuple(state.delta), -1 / scale.value))
    state.event.stopImmediatePropagation()
  }

  function handleDragZooming(state: DragState) {
    const prevScale = scale.value
    updateScale((oldValue) => oldValue * Math.exp(-state.delta[1] / 100))
    scrollTo(center.value.scaleAround(prevScale / scale.value, gesturePivot))
  }

  function handleDragHolding(state: DragState) {
    callHoldDragListeners({
      ...state,
      first: holdDragStarted === false,
      dragging: state.dragging || !state.last,
    })
    holdDragStarted = true
  }

  let longpressTimer: ReturnType<typeof setTimeout> | null = null
  function startTouchLongpressTimer(state: DragState) {
    state.event.preventDefault()
    cancelLongpress()
    longpressTimer = setTimeout(() => {
      longpressTimer = null
      if (navigator.vibrate) navigator.vibrate(20)
      handleDragHolding(state)
    }, LONGPRESS_TIMEOUT)
  }

  function cancelLongpress() {
    if (longpressTimer) clearTimeout(longpressTimer)
    longpressTimer = null
  }

  useGesture(
    {
      onMove(state) {
        eventMousePos.value = Vec2.FromTuple(state.xy)
      },
      onDrag(state) {
        if (state.first) {
          gesturePivot = clientToScenePos(Vec2.FromTuple(state.initial))
          holdDragStarted = false
        }

        if (!dragPredicate(state.event) || state.pinching) return
        const isTouch = eventIsTouch(state.event)
        const mainDown = (state.buttons & PointerButtonMask.Main) != 0
        const secondaryDown = (state.buttons & PointerButtonMask.Secondary) != 0
        const auxDown = (state.buttons & PointerButtonMask.Auxiliary) != 0

        if (isTouch && state.first) startTouchLongpressTimer(state)

        if (!holdDragStarted && (isTouch || auxDown)) handleDragPanning(state)
        else if (!isTouch && secondaryDown) handleDragZooming(state)
        else if (state.tap || holdDragStarted || (!isTouch && mainDown)) handleDragHolding(state)

        if (state.last && longpressTimer) cancelLongpress()
        if (state.last && holdDragStarted) holdDragStarted = false
      },
      onPinch(state) {
        // A started longpress touch can transform into pinch without warning, make sure to clear the timeout.
        cancelLongpress()

        if (state.ctrlKey) return // We do our own touchpad handling below

        const currentOrigin = Vec2.FromTuple(state.origin)
        gesturePivot = clientToScenePos(currentOrigin)
        if (state.first) {
          pinchScaleRatio = scale.value / state.da[0]
          lastPinchOrigin = currentOrigin
        }

        const originDelta = currentOrigin.sub(lastPinchOrigin)
        lastPinchOrigin = currentOrigin

        const prevScale = scale.value
        updateScale((_) => pinchScaleRatio * state.da[0])
        scrollTo(
          center.value
            .scaleAround(prevScale / scale.value, gesturePivot)
            .addScaled(originDelta, -1 / scale.value),
        )
      },
      onWheel(state) {
        if (state.ctrlKey) return
        const delta = Vec2.FromTuple(state.delta)
        scrollTo(center.value.addScaled(delta, 1 / scale.value))
      },
    },
    {
      domTarget: viewportNode,
      eventOptions: {
        passive: false,
      },
      drag: {
        enabled: true,
        useTouch: 'ontouchstart' in document.documentElement && navigator.maxTouchPoints > 0,
      },
      pinch: {
        enabled: true,
      },
      wheel: {
        enabled: true,
      },
    },
  )

  watch(size, updateViewportRect, { immediate: true })

  const targetScale = ref(1)
  const scale = useApproach(targetScale)

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

  /**
   * As `panTo`, but also follow the points if the viewport size is changing.
   *
   * The following is working until manual panning by user input or until the next call to any `pan…` function.
   */
  function panToThenFollow(points: Partial<Vec2>[]) {
    targetPoints = points
    panToImpl(points)
  }

  /**
   * Pan to include the given prioritized list of coordinates.
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
    targetCenter.value = target.center().finiteOrZero()
  }

  /** Pan immediately to center the viewport at the given point, in scene coordinates. */
  function scrollTo(newCenter: Vec2) {
    resetTargetFollowing()
    targetCenter.value = newCenter.finiteOrZero()
    center.skip()
  }

  /** Set viewport center point and scale value immediately, skipping animations. */
  function setCenterAndScale(newCenter: Vec2, newScale: number) {
    resetTargetFollowing()
    targetCenter.value = newCenter.finiteOrZero()
    targetScale.value = newScale
    scale.skip()
    center.skip()
  }

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

  const eventMousePos = shallowRef<Vec2 | null>(null)
  const sceneMousePos = computed(() =>
    eventMousePos.value ? clientToScenePos(eventMousePos.value) : null,
  )

  /**
   * Clamp the value to the given bounds, except if it is already outside the bounds allow the new value to be less
   *  outside the bounds.
   */
  function directedClamp(oldValue: number, newValue: number, [min, max]: ScaleRange): number {
    if (!Number.isFinite(newValue)) return oldValue
    else if (!Number.isFinite(oldValue)) return Math.max(min, Math.min(newValue, max))
    else if (newValue > oldValue) return Math.min(max, newValue)
    else return Math.max(min, newValue)
  }

  function updateScale(f: (value: number) => number, range: ScaleRange = DEFAULT_SCALE_RANGE) {
    const oldValue = scale.value
    targetScale.value = directedClamp(oldValue, f(oldValue), range)
    scale.skip()
  }

  /**
   * Step to the next level from {@link ZOOM_LEVELS}.
   * @param zoomStepDelta step direction. If positive select larger zoom level; if negative  select smaller.
   * If 0, resets zoom level to 1.0.
   */
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
    targetCenter.value = center.value.add(scenePos0.sub(scenePos1)).finiteOrZero()
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

  useEventListener(viewportNode, 'wheel', wheelEvents.wheel)
  useEventListener(viewportNode, 'wheel', wheelEventsCapture.wheel, { capture: true })
  useEventListener(viewportNode, 'pointermove', wheelEventsCapture.pointermove, { capture: true })

  return proxyRefs({
    keyboardEvents: panArrows.events,
    translate: readonly(translate),
    targetCenter: readonly(targetCenter),
    targetScale: readonly(targetScale),
    scale: readonly(toRef(scale, 'value')),
    viewBox: readonly(viewBox),
    transform: readonly(transform),
    /**
     * Add handler for "hold" events - a drag action that doesn't move the navigator viewport, but
     * is supposed to represent holding the viewport contents itself. For desktop, this is the
     * left mouse button drag. For touch input, it is a long-press and drag.
     */
    addHoldDragListener(listener: Handler<'drag'>) {
      holdDragListeners.push(listener)
      onScopeDispose(() => holdDragListeners.splice(holdDragListeners.indexOf(listener), 1))
    },
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
