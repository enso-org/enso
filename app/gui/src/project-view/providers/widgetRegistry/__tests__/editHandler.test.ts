import { InteractionHandler } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { useCurrentEdit, type CurrentEdit } from '@/providers/widgetTree'
import { expect, test, vi, type Mock } from 'vitest'
import { proxyRefs } from 'vue'
import { assert } from 'ydoc-shared/util/assert'
import { WidgetEditHandler, type WidgetEditHooks } from '../editHandler'

// If widget's name is a prefix of another widget's name, then it is its ancestor.
// The ancestor with longest name is a direct parent.
function editHandlerTree(
  widgets: string[],
  interactionHandler: InteractionHandler,
  createInteraction: (name: PortId) => WidgetEditHooks & Record<string, Mock>,
  widgetTree: CurrentEdit,
): Map<
  string,
  { handler: WidgetEditHandler; interaction: WidgetEditHooks & Record<string, Mock> }
> {
  const handlers = new Map()
  for (const id of widgets) {
    let parent: string | undefined
    for (const [otherId] of handlers) {
      if (id.startsWith(otherId) && otherId.length > (parent?.length ?? -1)) parent = otherId
    }
    const portId = id as PortId
    const interaction = createInteraction(portId)
    const handler = new WidgetEditHandler(
      portId,
      interaction,
      parent ? handlers.get(parent)?.handler : undefined,
      widgetTree,
      interactionHandler,
    )
    handlers.set(id, { handler, interaction })
  }
  return handlers
}

test.each`
  widgets                     | edited   | expectedPropagation
  ${['A']}                    | ${'A'}   | ${['A']}
  ${['A', 'A1', 'B']}         | ${'A1'}  | ${['A', 'A1']}
  ${['A', 'A1', 'A2']}        | ${'A2'}  | ${['A', 'A2']}
  ${['A', 'A1', 'A11']}       | ${'A1'}  | ${['A', 'A1']}
  ${['A', 'A1', 'A11']}       | ${'A11'} | ${['A', 'A1', 'A11']}
  ${['A', 'A1', 'A2', 'A21']} | ${'A21'} | ${['A', 'A2', 'A21']}
`(
  'Edit interaction propagation starting from $edited in $widgets tree',
  ({ widgets, edited, expectedPropagation }) => {
    const interactionHandler = new InteractionHandler()
    const widgetTree = proxyRefs(useCurrentEdit())
    const handlers = editHandlerTree(
      widgets,
      interactionHandler,
      () => ({
        start: vi.fn(),
        edit: vi.fn(),
        end: vi.fn(),
        cancel: vi.fn(),
      }),
      widgetTree,
    )
    const expectedPropagationSet = new Set(expectedPropagation)
    const checkCallbackCall = (callback: string, ...args: any[]) => {
      for (const [id, { interaction }] of handlers) {
        if (expectedPropagationSet.has(id)) {
          expect(interaction[callback]).toHaveBeenCalledWith(...args)
        } else {
          expect(interaction[callback]).not.toHaveBeenCalled()
        }
        interaction[callback]?.mockClear()
      }
    }

    const editedHandler = handlers.get(edited)
    assert(editedHandler != null)

    editedHandler.handler.start()
    expect(widgetTree.currentEdit).toBe(editedHandler.handler)
    checkCallbackCall('start', edited)
    const handlersActive = [...handlers]
      .filter(([_id, { handler }]) => handler.isActive())
      .map(([id]) => id)
    expect(handlersActive.sort()).toEqual([...expectedPropagationSet].sort())

    editedHandler.handler.edit('13')
    checkCallbackCall('edit', edited, '13')

    for (const ended of expectedPropagation) {
      const endedHandler = handlers.get(ended)?.handler

      editedHandler.handler.start()
      expect(widgetTree.currentEdit).toBe(editedHandler.handler)
      expect(editedHandler.handler.isActive()).toBeTruthy()
      endedHandler?.end()
      expect(widgetTree.currentEdit).toBeUndefined()
      checkCallbackCall('end', ended)
      expect(editedHandler.handler.isActive()).toBeFalsy()

      editedHandler.handler.start()
      expect(widgetTree.currentEdit).toBe(editedHandler.handler)
      expect(editedHandler.handler.isActive()).toBeTruthy()
      endedHandler?.cancel()
      expect(widgetTree.currentEdit).toBeUndefined()
      checkCallbackCall('cancel')
      expect(editedHandler.handler.isActive()).toBeFalsy()
    }

    editedHandler.handler.start()
    expect(widgetTree.currentEdit).toBe(editedHandler.handler)
    expect(editedHandler.handler.isActive()).toBeTruthy()
    interactionHandler.setCurrent(undefined)
    expect(widgetTree.currentEdit).toBeUndefined()
    checkCallbackCall('end', undefined)
    expect(editedHandler.handler.isActive()).toBeFalsy()
  },
)

test.each`
  name                                | widgets               | edited   | propagatingHandlers | nonPropagatingHandlers | expectedHandlerCalls
  ${'Propagating'}                    | ${['A', 'A1']}        | ${'A1'}  | ${['A', 'A1']}      | ${[]}                  | ${['A', 'A1']}
  ${'Parent edited'}                  | ${['A', 'A1']}        | ${'A'}   | ${['A', 'A1']}      | ${[]}                  | ${['A']}
  ${'Not propagating'}                | ${['A', 'A1']}        | ${'A1'}  | ${['A1']}           | ${['A']}               | ${['A']}
  ${'Child only'}                     | ${['A', 'A1']}        | ${'A1'}  | ${['A1']}           | ${[]}                  | ${['A1']}
  ${'Skipping handler without click'} | ${['A', 'A1', 'A12']} | ${'A12'} | ${['A', 'A12']}     | ${[]}                  | ${['A', 'A12']}
  ${'Stopping propagation'}           | ${['A', 'A1', 'A12']} | ${'A12'} | ${['A', 'A12']}     | ${['A1']}              | ${['A', 'A1']}
`(
  'Handling clicks in WidgetEditHandlers case $name',
  ({ widgets, edited, propagatingHandlers, nonPropagatingHandlers, expectedHandlerCalls }) => {
    const event = new MouseEvent('pointerdown') as PointerEvent
    const interactionHandler = new InteractionHandler()
    const widgetTree = proxyRefs(useCurrentEdit())

    const propagatingHandlersSet = new Set(propagatingHandlers)
    const nonPropagatingHandlersSet = new Set(nonPropagatingHandlers)
    const expectedHandlerCallsSet = new Set(expectedHandlerCalls)

    const handlers = editHandlerTree(
      widgets,
      interactionHandler,
      (id) =>
        propagatingHandlersSet.has(id) ?
          {
            pointerdown: vi.fn((e) => {
              expect(e).toBe(event)
              return false
            }),
          }
        : nonPropagatingHandlersSet.has(id) ?
          {
            pointerdown: vi.fn((e) => {
              expect(e).toBe(event)
            }),
          }
        : {},
      widgetTree,
    )
    handlers.get(edited)?.handler.start()
    interactionHandler.handlePointerEvent(event, 'pointerdown')
    const handlersCalled = new Set<string>()
    for (const [id, { interaction }] of handlers)
      if ((interaction.pointerdown as Mock | undefined)?.mock.lastCall) handlersCalled.add(id)
    expect([...handlersCalled].sort()).toEqual([...expectedHandlerCallsSet].sort())
  },
)
