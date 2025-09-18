import { describe, expect, test, vi } from 'vitest'
import { nextTick, ref } from 'vue'
import { InteractionHandler, type Interaction } from '../interactionHandler'

function mockInteraction(interaction: Partial<Interaction>) {
  const onPointerDown = vi.fn(() => {})
  const onCancel = vi.fn(() => {})
  const onEnd = vi.fn(() => {})
  return {
    interaction: {
      pointerdown: onPointerDown,
      cancel: onCancel,
      end: onEnd,
      ...interaction,
    },
    onPointerDown,
    onCancel,
    onEnd,
  }
}

function setupNested() {
  const parent = mockInteraction({})
  const handler = new InteractionHandler()
  const child = mockInteraction({
    parentInteraction: parent.interaction,
  })
  return { handler, parent, child }
}

test('Handle pointer events', () => {
  const { interaction, onPointerDown } = mockInteraction({})
  const handler = new InteractionHandler()
  handler.setCurrent(interaction)
  handler.handlePointerDown({} as PointerEvent)
  expect(onPointerDown).toHaveBeenCalled()
})

describe('Cancelling interactions', () => {
  test('Single interaction', () => {
    const { interaction, onCancel } = mockInteraction({})
    const handler = new InteractionHandler()
    handler.cancel(interaction)
    handler.setCurrent(interaction)
    handler.cancel(interaction)
    expect(onCancel).toBeCalledTimes(1)
  })

  test('Cancel child', () => {
    const { handler, parent, child } = setupNested()
    handler.setCurrent(child.interaction)
    handler.cancel(child.interaction)
    expect(parent.onCancel).not.toBeCalled()
    expect(child.onCancel).toBeCalledTimes(1)
    expect(handler.getCurrent()).toEqual(parent.interaction)
  })

  test('Cancel parent', () => {
    const { handler, parent, child } = setupNested()
    handler.setCurrent(child.interaction)
    handler.cancel(parent.interaction)
    expect(parent.onCancel).toBeCalledTimes(1)
    expect(child.onCancel).toBeCalledTimes(1)
    expect(handler.getCurrent()).toBeUndefined()
  })

  test('Cancel all', () => {
    const { handler, child, parent } = setupNested()
    // Single interaction
    expect(handler.cancelAll()).toBe(false)
    handler.setCurrent(parent.interaction)
    expect(handler.cancelAll()).toBe(true)
    expect(parent.onCancel).toBeCalledTimes(1)

    // Nested interactions
    handler.setCurrent(child.interaction)
    expect(handler.cancelAll()).toBe(true)
    expect(parent.onCancel).toBeCalledTimes(2)
    expect(child.onCancel).toBeCalledTimes(1)
    expect(handler.getCurrent()).toBeUndefined()
  })
})

describe('Ending interactions', () => {
  test('Single interaction', () => {
    const { interaction, onEnd } = mockInteraction({})
    const handler = new InteractionHandler()
    handler.end(interaction)
    expect(onEnd).not.toBeCalled()
    handler.setCurrent(interaction)
    handler.end(interaction)
    expect(onEnd).toBeCalledTimes(1)
  })

  test('End child', () => {
    const { handler, parent, child } = setupNested()
    handler.setCurrent(child.interaction)
    handler.end(child.interaction)
    expect(parent.onEnd).not.toBeCalled()
    expect(child.onEnd).toBeCalledTimes(1)
    expect(handler.getCurrent()).toEqual(parent.interaction)
  })

  test('End parent', () => {
    const { handler, parent, child } = setupNested()
    handler.setCurrent(child.interaction)
    handler.end(parent.interaction)
    expect(parent.onEnd).toBeCalledTimes(1)
    expect(child.onEnd).toBeCalledTimes(1)
    expect(handler.getCurrent()).toBeUndefined()
  })
})

test('Set current', () => {
  const { handler, parent, child } = setupNested()
  handler.setCurrent(parent.interaction)
  expect(handler.getCurrent()).toBe(parent.interaction)

  handler.setCurrent(child.interaction)
  expect(handler.getCurrent()).toBe(child.interaction)
  expect(parent.onEnd).not.toBeCalled()
  expect(child.onEnd).not.toBeCalled()

  const { interaction: anotherInteraction } = mockInteraction({})
  handler.setCurrent(anotherInteraction)
  expect(handler.getCurrent()).toBe(anotherInteraction)
  expect(parent.onEnd).toBeCalledTimes(1)
  expect(child.onEnd).toBeCalledTimes(1)
})

test('setWhen', async () => {
  const { interaction } = mockInteraction({})
  const handler = new InteractionHandler()
  const condition = ref(false)
  handler.setWhen(condition, interaction)
  expect(handler.getCurrent()).toBeUndefined()
  condition.value = true
  await nextTick()
  expect(handler.getCurrent()).toBe(interaction)
  condition.value = false
  await nextTick()
  expect(handler.getCurrent()).toBeUndefined()

  const { interaction: secondInteraction, onEnd } = mockInteraction({})
  handler.setCurrent(secondInteraction)
  condition.value = true
  await nextTick()
  expect(handler.getCurrent()).toBe(interaction)
  expect(onEnd).toHaveBeenCalled()
})

test('setWhenWithParent', async () => {
  const { handler, parent, child } = setupNested()
  const condition = ref(false)
  handler.setWhenWithParent(condition, (p) => {
    expect(p).toBe(parent.interaction)
    return child.interaction
  })

  handler.setCurrent(parent.interaction)
  condition.value = true
  await nextTick()
  expect(handler.getCurrent()).toStrictEqual(child.interaction)
  expect(parent.onEnd).not.toBeCalled()
  expect(child.onEnd).not.toBeCalled()

  condition.value = false
  await nextTick()
  expect(handler.getCurrent()).toBe(parent.interaction)
  expect(parent.onEnd).not.toBeCalled()
  expect(child.onEnd).toBeCalledTimes(1)
})
