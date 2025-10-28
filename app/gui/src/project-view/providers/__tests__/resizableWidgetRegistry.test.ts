import { type PortId } from '@/providers/portInfo'
import { useResizableWidgetRegistry } from '@/providers/resizableWidgetRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { assert, expect, test } from 'vitest'
import { nextTick, ref } from 'vue'

const NODE_PADDING = 4

test.each`
  nodeWidth | widgetTreeDomWidth | widgetWidth | widgetDomWidth | expected | commentary
  ${200}    | ${100}             | ${70}       | ${70}          | ${162}   | ${'Basic case'}
  ${200}    | ${192}             | ${120}      | ${120}         | ${120}   | ${'Already valid'}
  ${200}    | ${100}             | ${120}      | ${28}          | ${120}   | ${'Already valid, but dom not refreshed'}
`(
  'Initializing resizable widget width: $commentary',
  async ({ nodeWidth, widgetTreeDomWidth, widgetWidth, widgetDomWidth, expected }) => {
    const nodeWidthRef = ref(nodeWidth)
    const { register, unregister } = useResizableWidgetRegistry(
      nodeWidthRef,
      NODE_PADDING,
      widgetTreeDomWidth,
    )
    const widgetSizeRef = ref(Rect.XYWH(0, 0, widgetWidth, 100))
    const widgetDomSizeRef = ref(new Vec2(widgetDomWidth, 100))
    register('SingleWidget' as PortId, widgetSizeRef, widgetDomSizeRef)
    await nextTick()
    expect(nodeWidthRef.value).toBe(nodeWidth)
    expect(widgetSizeRef.value).toEqual(Rect.XYWH(0, 0, expected, 100))
    unregister('SingleWidget' as PortId)

    // If there's more than one widget, they should be not resized.
    const widget1SizeRef = ref(Rect.XYWH(0, 0, widgetWidth / 2, 100))
    const widget1DomSizeRef = ref(new Vec2(widgetDomWidth / 2, 100))
    const widget2SizeRef = ref(Rect.XYWH(0, 0, widgetWidth / 2, 100))
    const widget2DomSizeRef = ref(new Vec2(widgetDomWidth / 2, 100))
    register('Widget1' as PortId, widget1SizeRef, widget1DomSizeRef)
    register('Widget2' as PortId, widget2SizeRef, widget2DomSizeRef)
    await nextTick()
    expect(nodeWidthRef.value).toBe(nodeWidth)
    expect(widget1SizeRef.value).toEqual(Rect.XYWH(0, 0, widgetWidth / 2, 100))
    expect(widget2SizeRef.value).toEqual(Rect.XYWH(0, 0, widgetWidth / 2, 100))
  },
)

test.each([[[20]], [[-20]], [[10, 10]], [[-10, -10]]])(
  'Resizing visualization in %s steps updates the single resizable widget',
  async (resizingSteps) => {
    const nodeWidthRef = ref(100)
    const { register, visResizeHandleEventHandlers } = useResizableWidgetRegistry(
      nodeWidthRef,
      NODE_PADDING,
      nodeWidthRef.value - 2 * NODE_PADDING,
    )
    // If someone would add this event in the future, it should be tested.
    assert(!('update:resizing' in visResizeHandleEventHandlers))

    const widgetSizeRef = ref(Rect.XYWH(0, 0, 80, 100))
    const widgetDomSizeRef = ref(new Vec2(80, 100))
    register('SingleWidget' as PortId, widgetSizeRef, widgetDomSizeRef)
    await nextTick()

    for (const step of resizingSteps) {
      const newNodeWidth = nodeWidthRef.value + step
      const newWidgetSizeRef = Rect.XYWH(
        0,
        0,
        widgetSizeRef.value.width + step,
        widgetSizeRef.value.height,
      )
      visResizeHandleEventHandlers['update:modelValue'](Rect.XYWH(123, 456, newNodeWidth, 108))
      nodeWidthRef.value = newNodeWidth
      await nextTick()
      expect(widgetSizeRef.value).toEqual(newWidgetSizeRef)
    }
  },
)

test.each([[[20]], [[-20]], [[10, 10]], [[-10, -10]]])(
  'Resizing single widget in %s steps updates visualization width',
  async (resizingSteps) => {
    const nodeWidthRef = ref(100)
    const { register, widgetResizeHandleEventHandlers } = useResizableWidgetRegistry(
      nodeWidthRef,
      NODE_PADDING,
      nodeWidthRef.value - 2 * NODE_PADDING,
    )
    const widgetSizeRef = ref(Rect.XYWH(0, 0, 80, 100))
    const widgetDomSizeRef = ref(new Vec2(80, 100))
    register('SingleWidget' as PortId, widgetSizeRef, widgetDomSizeRef)
    await nextTick()

    widgetResizeHandleEventHandlers['update:resizing']({ right: true })
    let delta = 0
    for (const step of resizingSteps) {
      delta += step
      const newWidgetSize = Rect.XYWH(
        0,
        0,
        widgetSizeRef.value.width + step,
        widgetSizeRef.value.height,
      )
      const newNodeWidth = nodeWidthRef.value + step
      widgetResizeHandleEventHandlers['update:modelValue'](newWidgetSize, new Vec2(delta, 0))
      widgetSizeRef.value = newWidgetSize
      await nextTick()
      expect(nodeWidthRef.value).toEqual(newNodeWidth)
    }
  },
)
