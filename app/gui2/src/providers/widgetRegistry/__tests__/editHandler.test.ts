import type { GraphNavigator } from '@/providers/graphNavigator'
import { InteractionHandler } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { assert } from 'shared/util/assert'
import { expect, test, vi, type Mock } from 'vitest'
import { WidgetEditHandler } from '../editHandler'

// If widget's name is a prefix of another widget's name, then it is its ancestor.
// The ancestor with longest name is a direct parent.
function editHandlerTree(
  widgets: string[],
  interactionHandler: InteractionHandler,
  createInteraction: (name: string) => Record<string, Mock>,
  widgetTree: { currentEdit: WidgetEditHandler | undefined },
): Map<string, { handler: WidgetEditHandler; interaction: Record<string, Mock> }> {
  const handlers = new Map()
  for (const id of widgets) {
    let parent: string | undefined
    for (const [otherId] of handlers) {
      if (id.startsWith(otherId) && otherId.length > (parent?.length ?? -1)) parent = otherId
    }
    const interaction = createInteraction(id)
    const handler = new WidgetEditHandler(
      id as PortId,
      interaction,
      parent ? handlers.get(parent)?.handler : undefined,
      interactionHandler,
      widgetTree,
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
    const widgetTree = { currentEdit: undefined }
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
    for (const [id, { handler }] of handlers) {
      expect(handler.isActive()).toBe(expectedPropagationSet.has(id))
    }

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
    checkCallbackCall('cancel')
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
    const event = new MouseEvent('click') as PointerEvent
    const navigator = {} as GraphNavigator
    const interactionHandler = new InteractionHandler()
    const widgetTree = { currentEdit: undefined }

    const propagatingHandlersSet = new Set(propagatingHandlers)
    const nonPropagatingHandlersSet = new Set(nonPropagatingHandlers)
    const expectedHandlerCallsSet = new Set(expectedHandlerCalls)

    const handlers = editHandlerTree(
      widgets,
      interactionHandler,
      (id) =>
        propagatingHandlersSet.has(id) ?
          {
            click: vi.fn((e, nav, childHandler) => {
              expect(e).toBe(event)
              expect(nav).toBe(navigator)
              childHandler?.()
            }),
          }
        : nonPropagatingHandlersSet.has(id) ?
          {
            click: vi.fn((e, nav) => {
              expect(e).toBe(event)
              expect(nav).toBe(navigator)
            }),
          }
        : {},
      widgetTree,
    )
    handlers.get(edited)?.handler.start()
    interactionHandler.handleClick(event, navigator)
    for (const [id, { interaction }] of handlers) {
      if (expectedHandlerCallsSet.has(id))
        expect(interaction.click, `${id} click handler`).toHaveBeenCalled()
      else if (interaction.click)
        expect(interaction.click, `${id} click handler`).not.toHaveBeenCalled()
    }
  },
)
