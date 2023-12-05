import CircularMenu from '@/components/CircularMenu.vue'
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import type { Tree } from '@/generated/ast'
import type { GraphNavigator } from '@/providers/graphNavigator'
import type { GraphSelection } from '@/providers/graphSelection'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { GraphDb } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import { ComputedValueRegistry } from '@/stores/project/computedValueRegistry'
import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import { AstExtended } from '@/util/ast'
import { nodeFromAst } from '@/util/ast/node'
import { PointerButtonMask } from '@/util/events'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { createTestingPinia } from '@pinia/testing'
import { VueWrapper, mount } from '@vue/test-utils'
import { IdMap, type ContentRange } from 'shared/yjsModel'
import { expect, test, vi } from 'vitest'
import { computed, nextTick, ref, watchEffect } from 'vue'
import * as Y from 'yjs'

// It is currently not feasible to use generics here, as the type of the component's emits
// is not exposed.
function handleEmit(wrapper: VueWrapper<any>, event: string, fn: (...args: any[]) => void) {
  let previousLength = 0
  return {
    async run() {
      const emitted = wrapper.emitted(event)
      if (!emitted) return
      for (let i = previousLength; i < emitted.length; i += 1) {
        fn(...emitted[i]!)
      }
      previousLength = emitted.length
      await nextTick()
    },
  }
}

const INITIAL_NODE_CONTENT = 'a.b + c.d'

test('overriding output context', async () => {
  const doc = new Y.Doc()
  const yIds = doc.getMap<Uint8Array>('ids')
  const yCode = doc.getText('text')
  const idMap = new IdMap(yIds, yCode)
  const text = ref(INITIAL_NODE_CONTENT)
  const ast = computed(
    () => AstExtended.parse(text.value, idMap).children()[1]! as AstExtended<Tree>,
  )
  const node = computed(() => nodeFromAst(ast.value))
  const suggestionDb = new SuggestionDb()
  const groups = ref<Group[]>([])
  const valuesRegistry = ComputedValueRegistry.Mock()
  const graphDb = new GraphDb(suggestionDb, groups, valuesRegistry)
  const pinia = createTestingPinia({ createSpy: vi.fn, stubActions: false })
  const component = mount(GraphNode, {
    props: { node: node.value, edited: false },
    global: {
      plugins: [
        pinia,
        (app) =>
          provideGuiConfig._mock(
            ref({
              startup: {
                project: 'Mock Project',
                displayedProjectName: 'Mock Project',
              },
              engine: { rpcUrl: 'mock://engine', dataUrl: 'mock://data' },
            }),
            app,
          ),
        (app) => provideWidgetRegistry._mock([graphDb], app),
        () => {
          const projectStore = useProjectStore(pinia)
          const mod = projectStore.projectModel.createNewModule('Main.enso')
          mod.doc.ydoc.emit('load', [])
        },
      ],
      provide: {
        'graph navigator': {
          events: {} as any,
          clientToScenePos: () => Vec2.Zero,
          clientToSceneRect: () => Rect.Zero,
          transform: '',
          prescaledTransform: '',
          translate: Vec2.Zero,
          scale: 1,
          sceneMousePos: Vec2.Zero,
          viewBox: '',
          viewport: Rect.Zero,
        } satisfies GraphNavigator,
        'graph selection': {
          events: {} as any,
          anchor: undefined,
          deselectAll: () => {},
          addHoveredPort: () => new Set(),
          removeHoveredPort: () => false,
          handleSelectionOf: () => {},
          hoveredNode: undefined,
          hoveredPort: undefined,
          // Required for the `CircularMenu` to be visible.
          isSelected: () => true,
          mouseHandler: () => false,
          selectAll: () => {},
          selected: new Set(),
        } satisfies GraphSelection,
      },
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
