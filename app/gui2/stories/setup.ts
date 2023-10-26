import '@/assets/base.css'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { MockTransport } from '@/util/net'
import type { QualifiedName } from '@/util/qualifiedName'
import { Vec2 } from '@/util/vec2'
import { defineSetupVue3 } from '@histoire/plugin-vue'
import { uuidv4 } from 'lib0/random'
import { createPinia } from 'pinia'
import type { LibraryComponentGroup, Uuid, response } from 'shared/languageServerTypes'
import type { SuggestionEntry } from 'shared/languageServerTypes/suggestions'
import { ref } from 'vue'
import mockDb from './mockSuggestions.json' assert { type: 'json' }
import './story.css'

const mockProjectId = uuidv4() as Uuid
const standardBase = 'Standard.Base' as QualifiedName

export function placeholderGroups(): LibraryComponentGroup[] {
  return [
    { color: '#4D9A29', name: 'Input', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Web', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Parse', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Select', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Join', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Transform', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Output', library: standardBase, exports: [] },
  ]
}

MockTransport.addMock('engine', async (method, data, transport) => {
  switch (method) {
    case 'session/initProtocolConnection':
      return {
        contentRoots: [{ type: 'Project', id: mockProjectId }],
      } satisfies response.InitProtocolConnection
    case 'executionContext/create':
      setTimeout(
        () => transport.emit('executionContext/executionComplete', { contextId: data.contextId }),
        100,
      )
      return {
        contextId: data.contextId,
      }
    case 'search/getSuggestionsDatabase':
      return {
        entries: mockDb.map((suggestion, id) => ({
          id,
          suggestion: suggestion as SuggestionEntry,
        })),
        currentVersion: 1,
      } satisfies response.GetSuggestionsDatabase
    case 'runtime/getComponentGroups':
      return { componentGroups: placeholderGroups() } satisfies response.GetComponentGroups
    case 'executionContext/push':
    case 'executionContext/pop':
    case 'executionContext/recompute':
    case 'capability/acquire':
      return {}
    default:
      return Promise.reject(`Method not mocked: ${method}`)
  }
})

export const setupVue3 = defineSetupVue3(({ app }) => {
  app.use(createPinia())
  provideGuiConfig._mock(
    ref({
      startup: {
        project: 'Mock Project',
        displayedProjectName: 'Mock Project',
      },
      engine: { rpcUrl: 'mock://engine', dataUrl: 'mock://data' },
    }),
    app,
  )
  // Required for visualization stories.
  provideVisualizationConfig._mock(
    {
      fullscreen: false,
      width: 200,
      height: 150,
      hide() {},
      isCircularMenuVisible: false,
      nodeSize: new Vec2(200, 150),
      currentType: {
        module: { kind: 'Builtin' },
        name: 'Current Type',
      },
      types: [
        {
          module: { kind: 'Builtin' },
          name: 'Example',
        },
        {
          module: { kind: 'Builtin' },
          name: 'Types',
        },
        {
          module: { kind: 'Builtin' },
          name: 'Here',
        },
      ],
      updateType() {},
    },
    app,
  )
})
