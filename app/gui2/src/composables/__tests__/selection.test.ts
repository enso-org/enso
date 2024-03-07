import { useSelection } from '@/composables/selection'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
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

// TODO[ao]: We should read the modifiers from `bindings.ts`, but I don't know how yet.

// TODO[ao]: Skipping test, as they often fail in CI
// (for example https://github.com/enso-org/enso/actions/runs/8102076908/job/22163122663)
test.each`
  click | modifiers                  | expected
  ${1}  | ${[]}                      | ${[1]}
  ${3}  | ${[]}                      | ${[3]}
  ${1}  | ${['Shift']}               | ${[2]}
  ${3}  | ${['Shift']}               | ${[1, 2, 3]}
  ${1}  | ${['Mod', 'Shift']}        | ${[1, 2]}
  ${3}  | ${['Mod', 'Shift']}        | ${[1, 2, 3]}
  ${1}  | ${['Mod', 'Shift']}        | ${[1, 2]}
  ${3}  | ${['Mod', 'Shift']}        | ${[1, 2, 3]}
  ${1}  | ${['Alt', 'Shift']}        | ${[2]}
  ${3}  | ${['Alt', 'Shift']}        | ${[1, 2]}
  ${1}  | ${['Mod', 'Alt', 'Shift']} | ${[2]}
  ${3}  | ${['Mod', 'Alt', 'Shift']} | ${[1, 2, 3]}
`('Selection by single click at $click with $modifiers', ({ click, modifiers, expected }) => {
  const selection = selectionWithMockData()
  // Position is zero, because this method should not depend on click position
  selection.handleSelectionOf(mockPointerEvent('click', Vec2.Zero, modifiers), new Set([click]))
  expect(Array.from(selection.selected)).toEqual(expected)
})

const areas: Record<string, Rect> = {
  left: Rect.FromBounds(0, 0, 10, 30),
  right: Rect.FromBounds(20, 0, 30, 30),
  top: Rect.FromBounds(0, 0, 30, 10),
  bottom: Rect.FromBounds(0, 20, 30, 30),
  all: Rect.FromBounds(0, 0, 30, 30),
}

// TODO[ao]: Skipping test, as they often fail in CI
// (for example https://github.com/enso-org/enso/actions/runs/8102076908/job/22163122663)
test.each`
  areaId      | modifiers                  | expected
  ${'left'}   | ${[]}                      | ${[1, 3]}
  ${'right'}  | ${[]}                      | ${[2, 4]}
  ${'top'}    | ${[]}                      | ${[1, 2]}
  ${'bottom'} | ${[]}                      | ${[3, 4]}
  ${'all'}    | ${[]}                      | ${[1, 2, 3, 4]}
  ${'left'}   | ${['Shift']}               | ${[1, 2, 3]}
  ${'right'}  | ${['Shift']}               | ${[1, 2, 4]}
  ${'top'}    | ${['Shift']}               | ${[]}
  ${'bottom'} | ${['Shift']}               | ${[1, 2, 3, 4]}
  ${'all'}    | ${['Shift']}               | ${[1, 2, 3, 4]}
  ${'left'}   | ${['Mod', 'Shift']}        | ${[1, 2, 3]}
  ${'right'}  | ${['Mod', 'Shift']}        | ${[1, 2, 4]}
  ${'top'}    | ${['Mod', 'Shift']}        | ${[1, 2]}
  ${'bottom'} | ${['Mod', 'Shift']}        | ${[1, 2, 3, 4]}
  ${'all'}    | ${['Mod', 'Shift']}        | ${[1, 2, 3, 4]}
  ${'left'}   | ${['Alt', 'Shift']}        | ${[2]}
  ${'right'}  | ${['Alt', 'Shift']}        | ${[1]}
  ${'top'}    | ${['Alt', 'Shift']}        | ${[]}
  ${'bottom'} | ${['Alt', 'Shift']}        | ${[1, 2]}
  ${'all'}    | ${['Alt', 'Shift']}        | ${[]}
  ${'left'}   | ${['Mod', 'Alt', 'Shift']} | ${[2, 3]}
  ${'right'}  | ${['Mod', 'Alt', 'Shift']} | ${[1, 4]}
  ${'top'}    | ${['Mod', 'Alt', 'Shift']} | ${[]}
  ${'bottom'} | ${['Mod', 'Alt', 'Shift']} | ${[1, 2, 3, 4]}
  ${'all'}    | ${['Mod', 'Alt', 'Shift']} | ${[3, 4]}
`('Selection by dragging $area area with $modifiers', ({ areaId, modifiers, expected }) => {
  const area = areas[areaId]!
  const dragCase = (start: Vec2, stop: Vec2) => {
    const mousePos = ref(start)
    const selection = selectionWithMockData(mousePos)

    selection.events.pointerdown(mockPointerEvent('pointerdown', mousePos.value, modifiers))
    selection.events.pointermove(mockPointerEvent('pointermove', mousePos.value, modifiers))
    mousePos.value = stop
    selection.events.pointermove(mockPointerEvent('pointermove', mousePos.value, modifiers))
    expect(selection.selected).toEqual(new Set(expected))
    selection.events.pointerdown(mockPointerEvent('pointerup', mousePos.value, modifiers))
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
    vi.spyOn(this, 'currentTarget', 'get').mockReturnValue(options.currentTarget ?? null)
    this.pointerId = 4
  }
}

function mockPointerEvent(type: string, pos: Vec2, modifiers: string[]): PointerEvent {
  const modifiersSet = new Set(modifiers)
  const event = new MockPointerEvent(type, {
    altKey: modifiersSet.has('Alt'),
    ctrlKey: modifiersSet.has('Mod'),
    shiftKey: modifiersSet.has('Shift'),
    metaKey: modifiersSet.has('Meta'),
    clientX: pos.x,
    clientY: pos.y,
    button: 0,
    buttons: 1,
    currentTarget: document.createElement('div'),
  }) as PointerEvent
  return event
}
