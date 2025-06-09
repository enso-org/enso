import { ProjectId } from '#/services/Backend'
import { createContextStore } from '@/providers'
import { WidgetRegistry } from '@/providers/widgetRegistry'
import { createGraphStore, GraphStore } from '@/stores/graph'
import { createProjectStore, LsUrls, ProjectStore } from '@/stores/project'
import { createProjectNameStore, ProjectNameStore } from '@/stores/projectNames'
import { createSuggestionDbStore, SuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert } from '@/util/assert'
import { ToValue } from '@/util/reactivity'
import { EffectScope, effectScope, shallowReactive } from 'vue'

/** All stores of a single opened project */
export interface OpenedProject {
  store: ProjectStore
  names: ProjectNameStore
  suggestionDb: SuggestionDbStore
  graph: GraphStore
  widgetRegistry: WidgetRegistry
}

/**
 * Properties of the project.
 *
 * This is a subset of ProjectView props which is used to set up the store.
 */
export interface ProjectProps {
  projectId: ProjectId
  projectNamespace: ToValue<string | undefined>
  projectInitialName: string
  projectDisplayedName: ToValue<string>
  renameProject: (newName: string) => void
  engine: LsUrls
}

/**
 * A type for Opened Project Store.
 */
export type OpenedProjectsStore = ReturnType<typeof injectOpenedProjects>

/**
 * Opened Projects Store
 *
 * This store maintains all "substores" of opened projects. When an opened project registers,
 * the names, project, suggestionDb, graph and widgetRegistry stores are created and available
 * through `get` method.
 *
 * See also `WithCurrentProject` component which allows setting one opened project as "default"
 * for component subtree.
 */
export const [provideOpenedProjects, injectOpenedProjects] = createContextStore(
  'opened-projects',
  () => {
    const projects = shallowReactive(
      new Map<string, OpenedProject & { storesScope: EffectScope }>(),
    )

    function registerProject(props: ProjectProps) {
      const { projectId } = props
      assert(!projects.has(projectId), 'Registering already registered project')
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

    function get(id: string): OpenedProject | undefined {
      return projects.get(id)
    }

    return {
      registerProject,
      projectClosed,
      get,
    }
  },
)
