import CircularMenu from '@/components/CircularMenu.vue'
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { nodeFromAst } from '@/util/ast/node'
import { PointerButtonMask } from '@/util/events'
import { MockTransport, MockWebSocket } from '@/util/net'
import { handleEmit } from '@/util/test/vue'
import { createTestingPinia } from '@pinia/testing'
import { mount } from '@vue/test-utils'
import { type ContentRange } from 'shared/yjsModel'
import { expect, test, vi } from 'vitest'
import { computed, nextTick, ref, watchEffect } from 'vue'
import { mockDataHandler, mockLSHandler } from '../../../../mock/engine'
import { mockGuiConfig, mockWidgetRegistry } from '../../../../mock/provideFns'
import * as mockProviders from '../../../../mock/providers'

const INITIAL_NODE_CONTENT = 'a.b + c.d'

test('overriding output context', async () => {
  MockTransport.addMock('engine', mockLSHandler)
  MockWebSocket.addMock('data', mockDataHandler)
  const text = ref(INITIAL_NODE_CONTENT)
  const ast = computed(() => Ast.parseLine(text.value))
  const node = computed(() => nodeFromAst(ast.value))
  const pinia = createTestingPinia({ createSpy: vi.fn, stubActions: false })
  const component = mount(GraphNode, {
    props: { node: node.value, edited: false },
    global: {
      plugins: [
        pinia,
        mockGuiConfig,
        mockWidgetRegistry,
        () => {
          const projectStore = useProjectStore(pinia)
          const mod = projectStore.projectModel.createNewModule('Main.enso')
          mod.doc.ydoc.emit('load', [])
        },
      ],
      provide: mockProviders.all,
    },
  })
  watchEffect(() => component.setProps({ node: node.value }))
  const handleUpdateContent = handleEmit(
    component,
    'update:content',
    (updates: [range: ContentRange, content: string][]) => {
      let newText = text.value
      for (const [[start, end], content] of [...updates].reverse()) {
        newText = newText.slice(0, start) + content + text.value.slice(end)
      }
      text.value = newText
    },
  )
  const selection = component.find('.selection')
  await selection.trigger('pointerdown', {
    buttons: PointerButtonMask.Main,
    clientX: 0,
    clientY: 0,
  })
  await selection.trigger('pointerup', { clientX: 0, clientY: 0 })
  const circularMenu = component.findComponent(CircularMenu)
  const contextOverrideIcon = circularMenu.find('[alt="Enable output context"]')

  // Initial state
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override enabled
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'design' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override disabled
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override enabled again; switch to "live" execution mode
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  const projectStore = useProjectStore(pinia)
  projectStore.executionMode = 'live'
  await nextTick()
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'design' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override enabled
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output 'live' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')

  // Override disabled
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  expect(text.value).toBe(INITIAL_NODE_CONTENT)
  expect(contextOverrideIcon.attributes('alt')).toBe('Disable output context')

  // Override enabled from disabled state
  await contextOverrideIcon.trigger('pointerdown')
  await handleUpdateContent.run()
  expect(text.value).toBe(
    "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output 'live' <| " +
      INITIAL_NODE_CONTENT,
  )
  expect(contextOverrideIcon.attributes('alt')).toBe('Enable output context')
})
