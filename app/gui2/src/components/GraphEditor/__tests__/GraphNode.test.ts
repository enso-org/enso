import CircularMenu from '@/components/CircularMenu.vue'
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { PointerButtonMask } from '@/util/events'
import { Vec2 } from '@/util/vec2'
import { createTestingPinia } from '@pinia/testing'
import { mount } from '@vue/test-utils'
import { expect, test, vi } from 'vitest'
import { computed, nextTick, watchEffect } from 'vue'
import * as mock from '../../../../mock'

const INITIAL_NODE_CONTENT = 'a.b + c.d'

test('overriding output context', async () => {
  // NOTE: Tests are broken because `replaceExpressionContent` discards the old node ID.
  mock.languageServer()
  mock.dataServer()
  const pinia = createTestingPinia({ createSpy: vi.fn, stubActions: false })

  const originalNode = mock.node()

  const component = mount(GraphNode, {
    props: { node: originalNode, edited: false },
    global: {
      plugins: [pinia, mock.guiConfig, mock.widgetRegistry, mock.projectStoreAndGraphStore],
      provide: mock.providers.allWith({
        'graph selection': {
          // Required for the `CircularMenu` to be visible.
          isSelected: () => true,
        },
      }),
    },
  })

  const graphStore = useGraphStore()
  const projectStore = useProjectStore()

  await mock.waitForMainModule()

  const exprId = graphStore.createNode(Vec2.Zero, INITIAL_NODE_CONTENT)!
  graphStore.db.mockNode('a', exprId, INITIAL_NODE_CONTENT)
  expect(exprId).toBeDefined()

  const node = computed(() => graphStore.db.nodeIdToNode.get(exprId)!)
  expect(node.value).toBeDefined()
  watchEffect(() => component.setProps(node.value != null ? { node: node.value } : {}))
  const text = computed(() => node.value?.rootSpan.code())

  // Testing start
  const selection = component.find('.selection')
  expect(selection).toBeDefined()
  await selection.trigger('pointerdown', {
    buttons: PointerButtonMask.Main,
    clientX: 0,
    clientY: 0,
  })
  await selection.trigger('pointerup', { clientX: 0, clientY: 0 })
  const circularMenu = component.findComponent(CircularMenu)
  expect(circularMenu).toBeDefined()
  const contextOverrideIcon = circularMenu.find('[alt="Enable output context"]')
  expect(contextOverrideIcon).toBeDefined()

  // Initial state
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override enabled
  await contextOverrideIcon.trigger('pointerdown')
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'design' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override disabled
  await contextOverrideIcon.trigger('pointerdown')
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override enabled again; switch to "live" execution mode
  await contextOverrideIcon.trigger('pointerdown')
  projectStore.executionMode = 'live'
  await nextTick()
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'design' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override enabled
  await contextOverrideIcon.trigger('pointerdown')
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output 'live' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override disabled
  await contextOverrideIcon.trigger('pointerdown')
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override enabled from disabled state
  await contextOverrideIcon.trigger('pointerdown')
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output 'live' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')
})
