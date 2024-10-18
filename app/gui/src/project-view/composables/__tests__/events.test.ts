import { Vec2 } from '@/util/data/vec2'
import { withSetup } from '@/util/testing'
import { afterEach, beforeEach, expect, test, vi, type Mock, type MockInstance } from 'vitest'
import { nextTick } from 'vue'
import { useArrows } from '../events'

let rafSpy: MockInstance
const rafCallbacks: FrameRequestCallback[] = []
beforeEach(() => {
  rafCallbacks.length = 0
  rafSpy = vi
    .spyOn(window, 'requestAnimationFrame')
    .mockImplementation((cb) => rafCallbacks.push(cb))
})
afterEach(() => {
  if (rafCallbacks.length > 0) {
    runFrame(Infinity)
    expect(rafCallbacks, 'Some RAF callbacks leaked from test').toEqual([])
  }
  rafSpy.mockRestore()
})

function runFrame(t: number) {
  const callbacks = rafCallbacks.splice(0, rafCallbacks.length)
  for (const cb of callbacks) {
    cb(t)
  }
}

function vecMatcher([x, y]: [number, number]) {
  return expect.objectContaining({
    x: expect.closeTo(x),
    y: expect.closeTo(y),
  })
}

function keyEvent(type: 'keydown' | 'keyup', options: KeyboardEventInit & { timeStamp?: number }) {
  const event = new KeyboardEvent(type, options)
  if (options.timeStamp != null) {
    vi.spyOn(event, 'timeStamp', 'get').mockReturnValue(options.timeStamp)
  }
  return event
}

type CbSequenceStep = [string, [number, number], [number, number], KeyboardEvent | undefined]
function checkCbSequence(cb: Mock, steps: CbSequenceStep[]) {
  let i = 1
  for (const [type, offset, delta, event] of steps) {
    expect(cb).toHaveBeenNthCalledWith(
      i++,
      {
        initial: Vec2.Zero,
        absolute: vecMatcher(offset),
        relative: vecMatcher(offset),
        delta: vecMatcher(delta),
      },
      type,
      event,
    )
  }
  expect(cb).toHaveBeenCalledTimes(steps.length)
}

test.each`
  pressedKeys                    | velocity | t0      | t                     | offset                                          | delta
  ${['ArrowRight']}              | ${10}    | ${2}    | ${[2, 3, 1002, 1003]} | ${[[0.0, 0.0], [0.01, 0], [10, 0], [10.01, 0]]} | ${[[0.0, 0.0], [0.01, 0], [9.99, 0], [0.01, 0]]}
  ${['ArrowLeft']}               | ${10}    | ${2}    | ${[2, 1002]}          | ${[[0.0, 0.0], [-10, 0]]}                       | ${[[0.0, 0.0], [-10, 0]]}
  ${['ArrowUp']}                 | ${10}    | ${2}    | ${[2, 1002]}          | ${[[0.0, 0.0], [0, -10]]}                       | ${[[0.0, 0.0], [0, -10]]}
  ${['ArrowDown']}               | ${20}    | ${2}    | ${[2, 3, 1002]}       | ${[[0.0, 0.0], [0, 0.02], [0, 20]]}             | ${[[0.0, 0.0], [0, 0.02], [0, 19.98]]}
  ${['ArrowRight', 'ArrowDown']} | ${10}    | ${1000} | ${[2000, 3000]}       | ${[[10, 10], [20, 20]]}                         | ${[[10, 10], [10, 10]]}
  ${['ArrowUp', 'ArrowLeft']}    | ${10}    | ${1000} | ${[2000, 3000]}       | ${[[-10, -10], [-20, -20]]}                     | ${[[-10, -10], [-10, -10]]}
`(
  'useArrows with $pressedKeys keys and $velocity velocity',
  async ({
    pressedKeys,
    velocity,
    t0,
    t,
    offset,
    delta,
  }: {
    pressedKeys: string[]
    velocity: number
    t0: number
    t: number[]
    offset: [number, number][]
    delta: [number, number][]
  }) => {
    await withSetup(async () => {
      const cb = vi.fn()
      const expectedSequence: CbSequenceStep[] = []
      const arrows = useArrows(cb, { velocity })
      expect(arrows.moving.value).toBeFalsy()
      const keydownEvents = Array.from(pressedKeys, (key) =>
        keyEvent('keydown', { key, timeStamp: t0 }),
      )
      for (const event of keydownEvents) {
        arrows.events.keydown(event)
      }
      await nextTick()
      expectedSequence.push(['start', [0, 0], [0, 0], keydownEvents[0]])
      expect(arrows.moving.value).toBeTruthy()

      for (let i = 0; i < t.length - 1; ++i) {
        runFrame(t[i]!)
        await nextTick()
        expectedSequence.push(['move', offset[i]!, delta[i]!, undefined])
      }

      const keyupEvents = Array.from(pressedKeys, (key) =>
        keyEvent('keyup', { key, timeStamp: t[t.length - 1]! }),
      )
      for (const event of keyupEvents) {
        window.dispatchEvent(event)
      }
      await nextTick()
      expectedSequence.push([
        'stop',
        offset[offset.length - 1]!,
        delta[delta.length - 1]!,
        keyupEvents[keyupEvents.length - 1],
      ])
      expect(arrows.moving.value).toBeFalsy()
      checkCbSequence(cb, expectedSequence)
    })[0]
  },
)

test('useArrow with non-overlaping keystrokes', async () => {
  await withSetup(async () => {
    const cb = vi.fn()
    const arrows = useArrows(cb, { velocity: 10 })
    const rightDown = keyEvent('keydown', { key: 'ArrowRight', timeStamp: 0 })
    const rightUp = keyEvent('keyup', { key: 'ArrowRight', timeStamp: 1000 })
    const downDown = keyEvent('keydown', { key: 'ArrowDown', timeStamp: 2000 })
    const downUp = keyEvent('keyup', { key: 'ArrowDown', timeStamp: 3000 })
    arrows.events.keydown(rightDown)
    await nextTick()
    runFrame(500)
    await nextTick()
    window.dispatchEvent(rightUp)
    await nextTick()
    runFrame(1500)
    await nextTick()
    arrows.events.keydown(downDown)
    await nextTick()
    runFrame(2500)
    await nextTick()
    window.dispatchEvent(downUp)
    await nextTick()
    runFrame(3500)
    await nextTick()

    checkCbSequence(cb, [
      ['start', [0, 0], [0, 0], rightDown],
      ['move', [5, 0], [5, 0], undefined],
      ['stop', [10, 0], [5, 0], rightUp],
      ['start', [0, 0], [0, 0], downDown],
      ['move', [0, 5], [0, 5], undefined],
      ['stop', [0, 10], [0, 5], downUp],
    ])
  })[0]
})

test('useArrow with overlaping keystrokes', async () => {
  await withSetup(async () => {
    const cb = vi.fn()
    const arrows = useArrows(cb, { velocity: 10 })
    const rightDown = keyEvent('keydown', { key: 'ArrowRight', timeStamp: 0 })
    const rightUp = keyEvent('keyup', { key: 'ArrowRight', timeStamp: 2000 })
    const downDown = keyEvent('keydown', { key: 'ArrowDown', timeStamp: 1000 })
    const downUp = keyEvent('keyup', { key: 'ArrowDown', timeStamp: 3000 })
    arrows.events.keydown(rightDown)
    await nextTick()
    runFrame(500)
    await nextTick()
    arrows.events.keydown(downDown)
    await nextTick()
    runFrame(1500)
    await nextTick()
    window.dispatchEvent(rightUp)
    await nextTick()
    runFrame(2500)
    await nextTick()
    window.dispatchEvent(downUp)
    await nextTick()
    runFrame(3500)
    await nextTick()

    checkCbSequence(cb, [
      ['start', [0, 0], [0, 0], rightDown],
      ['move', [5, 0], [5, 0], undefined],
      ['move', [15, 5], [10, 5], undefined],
      ['move', [20, 15], [5, 10], undefined],
      ['stop', [20, 20], [0, 5], downUp],
    ])
  })[0]
})
