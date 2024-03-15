import { selectionMouseBindings } from '@/bindings'
import { useSelection } from '@/composables/selection'
import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { isPointer, pointerButtonToEventInfo, type BindingInfo } from '@/util/shortcuts'
import { expect, test, vi } from 'vitest'
import { proxyRefs, ref, type Ref } from 'vue'

function selectionWithMockData(sceneMousePos?: Ref<Vec2>) {
  const rects = new Map()
  rects.set(1, Rect.FromBounds(1, 1, 10, 10))
  rects.set(2, Rect.FromBounds(20, 1, 30, 10))
  rects.set(3, Rect.FromBounds(1, 20, 10, 30))
  rects.set(4, Rect.FromBounds(20, 20, 30, 30))
  const navigator = proxyRefs({ sceneMousePos: sceneMousePos ?? ref(Vec2.Zero), scale: 1 })
  const selection = useSelection(navigator, rects, 0)
  selection.setSelection(new Set([1, 2]))
  return selection
}

const bindingReplace = selectionMouseBindings.bindings.replace
const bindingAdd = selectionMouseBindings.bindings.add
const bindingRemove = selectionMouseBindings.bindings.remove
const bindingToggle = selectionMouseBindings.bindings.toggle
const bindingInvert = selectionMouseBindings.bindings.invert

test.each`
  click | binding           | expected
  ${1}  | ${bindingReplace} | ${[1]}
  ${3}  | ${bindingReplace} | ${[3]}
  ${1}  | ${bindingToggle}  | ${[2]}
  ${3}  | ${bindingToggle}  | ${[1, 2, 3]}
  ${1}  | ${bindingAdd}     | ${[1, 2]}
  ${3}  | ${bindingAdd}     | ${[1, 2, 3]}
  ${1}  | ${bindingAdd}     | ${[1, 2]}
  ${3}  | ${bindingAdd}     | ${[1, 2, 3]}
  ${1}  | ${bindingRemove}  | ${[2]}
  ${3}  | ${bindingRemove}  | ${[1, 2]}
  ${1}  | ${bindingInvert}  | ${[2]}
  ${3}  | ${bindingInvert}  | ${[1, 2, 3]}
`('Selection by single click at $click with $modifiers', ({ click, binding, expected }) => {
  const selection = selectionWithMockData()
  // Position is zero, because this method should not depend on click position
  selection.handleSelectionOf(mockPointerEvent('click', Vec2.Zero, binding), new Set([click]))
  expect(Array.from(selection.selected)).toEqual(expected)
})

const areas: Record<string, Rect> = {
  left: Rect.FromBounds(0, 0, 10, 30),
  right: Rect.FromBounds(20, 0, 30, 30),
  top: Rect.FromBounds(0, 0, 30, 10),
  bottom: Rect.FromBounds(0, 20, 30, 30),
  all: Rect.FromBounds(0, 0, 30, 30),
}

test.each`
  areaId      | binding           | expected
  ${'left'}   | ${bindingReplace} | ${[1, 3]}
  ${'right'}  | ${bindingReplace} | ${[2, 4]}
  ${'top'}    | ${bindingReplace} | ${[1, 2]}
  ${'bottom'} | ${bindingReplace} | ${[3, 4]}
  ${'all'}    | ${bindingReplace} | ${[1, 2, 3, 4]}
  ${'left'}   | ${bindingToggle}  | ${[1, 2, 3]}
  ${'right'}  | ${bindingToggle}  | ${[1, 2, 4]}
  ${'top'}    | ${bindingToggle}  | ${[]}
  ${'bottom'} | ${bindingToggle}  | ${[1, 2, 3, 4]}
  ${'all'}    | ${bindingToggle}  | ${[1, 2, 3, 4]}
  ${'left'}   | ${bindingAdd}     | ${[1, 2, 3]}
  ${'right'}  | ${bindingAdd}     | ${[1, 2, 4]}
  ${'top'}    | ${bindingAdd}     | ${[1, 2]}
  ${'bottom'} | ${bindingAdd}     | ${[1, 2, 3, 4]}
  ${'all'}    | ${bindingAdd}     | ${[1, 2, 3, 4]}
  ${'left'}   | ${bindingRemove}  | ${[2]}
  ${'right'}  | ${bindingRemove}  | ${[1]}
  ${'top'}    | ${bindingRemove}  | ${[]}
  ${'bottom'} | ${bindingRemove}  | ${[1, 2]}
  ${'all'}    | ${bindingRemove}  | ${[]}
  ${'left'}   | ${bindingInvert}  | ${[2, 3]}
  ${'right'}  | ${bindingInvert}  | ${[1, 4]}
  ${'top'}    | ${bindingInvert}  | ${[]}
  ${'bottom'} | ${bindingInvert}  | ${[1, 2, 3, 4]}
  ${'all'}    | ${bindingInvert}  | ${[3, 4]}
`('Selection by dragging $area area with $modifiers', ({ areaId, binding, expected }) => {
  const area = areas[areaId]!
  const dragCase = (start: Vec2, stop: Vec2) => {
    const mousePos = ref(start)
    const selection = selectionWithMockData(mousePos)

    selection.events.pointerdown(mockPointerEvent('pointerdown', mousePos.value, binding))
    selection.events.pointermove(mockPointerEvent('pointermove', mousePos.value, binding))
    mousePos.value = stop
    selection.events.pointermove(mockPointerEvent('pointermove', mousePos.value, binding))
    expect(selection.selected).toEqual(new Set(expected))
    selection.events.pointerdown(mockPointerEvent('pointerup', mousePos.value, binding))
    expect(selection.selected).toEqual(new Set(expected))
  }

  // We should select same set of nodes, regardless of drag direction
  dragCase(new Vec2(area.left, area.top), new Vec2(area.right, area.bottom))
  dragCase(new Vec2(area.right, area.bottom), new Vec2(area.left, area.top))
  dragCase(new Vec2(area.left, area.bottom), new Vec2(area.right, area.top))
  dragCase(new Vec2(area.right, area.top), new Vec2(area.left, area.bottom))
})

// There is no PointerEvent class in jsdom (yet).
class MockPointerEvent extends MouseEvent {
  readonly pointerId: number
  constructor(type: string, options: MouseEventInit & { currentTarget?: Element | undefined }) {
    super(type, options)
    vi.spyOn<MouseEvent, 'currentTarget'>(this, 'currentTarget', 'get').mockReturnValue(
      options.currentTarget ?? null,
    )
    this.pointerId = 4
  }
}

function mockPointerEvent(type: string, pos: Vec2, binding: BindingInfo): PointerEvent {
  const modifiersSet = new Set(binding.modifiers)
  assert(isPointer(binding.key))
  const { button, buttons } = pointerButtonToEventInfo(binding.key)
  const event = new MockPointerEvent(type, {
    altKey: modifiersSet.has('Alt'),
    ctrlKey: modifiersSet.has('Mod'),
    shiftKey: modifiersSet.has('Shift'),
    metaKey: modifiersSet.has('Meta'),
    clientX: pos.x,
    clientY: pos.y,
    button,
    buttons,
    currentTarget: document.createElement('div'),
  }) as PointerEvent
  return event
}
