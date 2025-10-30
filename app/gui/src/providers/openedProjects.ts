import { ProjectId } from '#/services/Backend'
import { createGraphStore, type GraphStore } from '$/providers/openedProjects/graph'
import { createProjectStore, type ProjectStore } from '$/providers/openedProjects/project'
import {
  createProjectNameStore,
  type ProjectNameStore,
} from '$/providers/openedProjects/projectNames'
import {
  createSuggestionDbStore,
  type SuggestionDbStore,
} from '$/providers/openedProjects/suggestionDatabase'
import { WidgetRegistry } from '$/providers/openedProjects/widgetRegistry'
import { createContextStore } from '@/providers'
import { assert } from '@/util/assert'
import { EffectScope, effectScope, shallowReactive, toValue, type ToRefs } from 'vue'
import { createModuleStore, type ModuleStore } from './openedProjects/module'
import { type LsUrls } from './openedProjects/project/project'

/** All stores of a single opened project */
export interface OpenedProject {
  store: ProjectStore
  projectNames: ProjectNameStore
  suggestionDb: SuggestionDbStore
  module: ModuleStore
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
  projectNamespace: string | undefined
  projectInitialName: string
  projectDisplayedName: string
  renameProject: (newName: string) => Promise<void>
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
      new Map<ProjectId, OpenedProject & { storesScope: EffectScope }>(),
    )

    async function registerProject(props: ToRefs<ProjectProps>) {
      const { projectId, projectDisplayedName, projectNamespace } = props
      assert(!projects.has(toValue(projectId)), 'Registering already registered project')
      const storesScope = effectScope()

      storesScope.run(() => {
        const names = createProjectNameStore({
          projectNamespace,
          projectDisplayedName,
          projectInitialName: toValue(props.projectInitialName),
        })
        const store = createProjectStore(
          {
            projectId: toValue(projectId),
            renameProject: toValue(props.renameProject),
            engine: toValue(props.engine),
          },
          names,
        )
        const suggestionDb = createSuggestionDbStore(store, names)
        const module = createModuleStore(store, names, suggestionDb)
        const graph = createGraphStore(store, suggestionDb, names, module)
        const widgetRegistry = new WidgetRegistry(graph.db)
        projects.set(toValue(projectId), {
          projectNames: names,
          store,
          module,
          suggestionDb,
          graph,
          widgetRegistry,
          storesScope,
        })
      })
    }

    function unregisterProject(id: ProjectId) {
      projects.get(id)?.storesScope.stop()
      projects.delete(id)
    }

    function get(id: ProjectId): OpenedProject | undefined {
      return projects.get(id)
    }

    function listIds() {
      return projects.keys()
    }

    return {
      registerProject,
      unregisterProject,
      get,
      listIds,
    }
  },
)
