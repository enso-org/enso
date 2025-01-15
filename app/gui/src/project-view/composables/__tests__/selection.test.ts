import { selectionMouseBindings } from '@/bindings'
import { useSelection } from '@/composables/selection'
import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { isPointer, pointerButtonToEventInfo, type BindingInfo } from '@/util/shortcuts'
import { withSetup } from '@/util/testing'
import { beforeAll, expect, test, vi } from 'vitest'
import { nextTick, ref } from 'vue'
import { useKeyboard } from '../keyboard'
import { useNavigator } from '../navigator'

function selectionWithMockData() {
  const rects = new Map()
  rects.set(1, Rect.FromBounds(1, 1, 10, 10))
  rects.set(2, Rect.FromBounds(20, 1, 30, 10))
  rects.set(3, Rect.FromBounds(1, 20, 10, 30))
  rects.set(4, Rect.FromBounds(20, 20, 30, 30))

  const mockDom = document.createElement('div')
  mockDom.style.width = '500px'
  mockDom.style.height = '300px'
  vi.spyOn(mockDom, 'getBoundingClientRect').mockReturnValue(new DOMRect(-250, -150, 500, 300))

  const navigator = useNavigator(ref(mockDom), useKeyboard())

  const selection = useSelection(navigator, rects)
  selection.setSelection(new Set([1, 2]))
  return { selection, mockDom }
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
`('Selection by single click at $click', ({ click, binding, expected }) => {
  const [, app] = withSetup(() => {
    const { selection, mockDom } = selectionWithMockData()
    // Position is zero, because this method should not depend on click position
    selection.handleSelectionOf(
      mockPointerEvent('click', Vec2.Zero, binding, mockDom),
      new Set([click]),
    )
    expect([...selection.selected.value]).toEqual(expected)
  })
  app.unmount()
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
`(
  'Selection by dragging $areaId area with $binding.humanReadable',
  async ({ areaId, binding, expected }) => {
    const area = areas[areaId]!
    const dragCase = async (start: Vec2, stop: Vec2) => {
      const [test, app] = withSetup(async () => {
        const { selection, mockDom } = selectionWithMockData()
        await nextTick()
        mockDom.dispatchEvent(mockPointerEvent('pointerdown', start, binding, mockDom))
        mockDom.dispatchEvent(mockPointerEvent('pointermove', start, binding, mockDom))
        mockDom.dispatchEvent(mockPointerEvent('pointermove', stop, binding, mockDom))
        expect(selection.selected.value).toEqual(new Set(expected))
        mockDom.dispatchEvent(mockPointerEvent('pointerup', stop, binding, mockDom))
        expect(selection.selected.value).toEqual(new Set(expected))
      })
      await test
      app.unmount()
    }

    // We should select same set of nodes, regardless of drag direction
    await dragCase(new Vec2(area.left, area.top), new Vec2(area.right, area.bottom))
    await dragCase(new Vec2(area.right, area.bottom), new Vec2(area.left, area.top))
    await dragCase(new Vec2(area.left, area.bottom), new Vec2(area.right, area.top))
    await dragCase(new Vec2(area.right, area.top), new Vec2(area.left, area.bottom))
  },
)

// See https://github.com/thymikee/jest-preset-angular/issues/245#issuecomment-576296325
class MockPointerEvent extends MouseEvent {
  readonly pointerId: number
  constructor(
    type: string,
    options: MouseEventInit & { target?: Element | undefined; currentTarget?: Element | undefined },
  ) {
    super(type, options)
    vi.spyOn<MouseEvent, 'currentTarget'>(this, 'currentTarget', 'get').mockReturnValue(
      options.currentTarget ?? null,
    )
    vi.spyOn<MouseEvent, 'target'>(this, 'target', 'get').mockReturnValue(options.target ?? null)
    this.pointerId = 4
  }
}

beforeAll(() => {
  ;(window as any).PointerEvent = MockPointerEvent
})

let lastEventTimestamp = 0
function mockPointerEvent(
  type: string,
  pos: Vec2,
  binding: BindingInfo,
  target: Element,
): PointerEvent {
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
    target: target,
    currentTarget: target,
  }) as PointerEvent
  // Ensure that events sent during testing are not deduplicated
  if (event.timeStamp === lastEventTimestamp)
    Object.defineProperty(event, 'timeStamp', { value: event.timeStamp + 1 })
  lastEventTimestamp = event.timeStamp

  return event
}
