import { createContextStore } from '@/providers'
import { WidgetRegistry } from '@/providers/widgetRegistry'
import { createGraphStore, GraphStore } from '@/stores/graph'
import { createProjectStore, LsUrls, ProjectStore } from '@/stores/project'
import { createProjectNameStore, ProjectNameStore } from '@/stores/projectNames'
import { createSuggestionDbStore, SuggestionDbStore } from '@/stores/suggestionDatabase'
import { ToValue } from '@/util/reactivity'
import { EffectScope, effectScope, shallowReactive } from 'vue'

interface OpenedProject {
  store: ProjectStore
  names: ProjectNameStore
  suggestionDb: SuggestionDbStore
  graph: GraphStore
  widgetRegistry: WidgetRegistry
  storesScope: EffectScope
}

/**
 * Properties of the project.
 *
 * This is a subset of ProjectView props which is used to set up the store.
 */
export interface ProjectProps {
  projectId: string
  projectNamespace: ToValue<string | undefined>
  projectInitialName: string
  projectDisplayedName: ToValue<string>
  renameProject: (newName: string) => void
  engine: LsUrls
}

export type OpenedProjectsStore = ReturnType<typeof injectOpenedProjects>
export const [provideOpenedProjects, injectOpenedProjects] = createContextStore(
  'opened-projects',
  () => {
    const projects = shallowReactive(new Map<string, OpenedProject>())

    function registerProject(props: ProjectProps) {
      const { projectId } = props
      const storesScope = effectScope()

      storesScope.run(() => {
        const names = createProjectNameStore(props)
        const store = createProjectStore(props, names)
        const suggestionDb = createSuggestionDbStore(store, names)
        const graph = createGraphStore(store, suggestionDb, names)
        const widgetRegistry = new WidgetRegistry(graph.db)
        projects.set(projectId, { names, store, suggestionDb, graph, widgetRegistry, storesScope })
      })
    }

    function projectClosed(id: string) {
      projects.get(id)?.storesScope.stop()
      projects.delete(id)
    }

    function get(id: string) {
      return projects.get(id)
    }

    return {
      registerProject,
      projectClosed,
      get,
    }
  },
)
