import { provideGuiConfig } from '@/providers/guiConfig'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { GraphDb } from '@/stores/graph/graphDatabase'
import { ComputedValueRegistry } from '@/stores/project/computedValueRegistry'
import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import { ref, type App } from 'vue'

export function mockGuiConfig(app: App) {
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
}

export function mockWidgetRegistry(app: App) {
  const suggestionDb = new SuggestionDb()
  const groups = ref<Group[]>([])
  const valuesRegistry = ComputedValueRegistry.Mock()
  const graphDb = new GraphDb(suggestionDb, groups, valuesRegistry)
  provideWidgetRegistry._mock([graphDb], app)
}
