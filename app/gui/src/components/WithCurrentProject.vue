<script lang="ts">
import { useOpenedProjects } from '$/providers/openedProjects'
import type { Initialized as InitializedProject } from '$/providers/openedProjects/projectStates'
import { groupColorVar } from '@/composables/nodeColors'
import { createContextStore } from '@/providers'
import { assert } from '@/util/assert'
import { colorFromString } from '@/util/colors'
import type { Opt } from '@/util/data/opt'
import { Loader, ResultComponent } from '@/util/react'
import type { ProjectId } from 'enso-common/src/services/Backend'
import { computed, type Ref, shallowRef, watch } from 'vue'

export type CurrentProjectStore = ReturnType<typeof useCurrentProjectRaw>
const [provideCurrentProject, useCurrentProjectRaw] = createContextStore(
  'currentProject',
  (project: Ref<InitializedProject | undefined>) => {
    const ref = computed(() => {
      assert(project.value != null)
      return project.value
    })
    return {
      maybeRef: project,
      id: computed(() => ref.value.info.id),
      ensoPath: computed(() => ref.value.info.ensoPath),
      store: computed(() => ref.value.store),
      projectNames: computed(() => ref.value.projectNames),
      suggestionDb: computed(() => ref.value.suggestionDb),
      module: computed(() => ref.value.module),
      graph: computed(() => ref.value.graph),
      widgetRegistry: computed(() => ref.value.widgetRegistry),
    }
  },
)

export function useCurrentProject(allowMissing: true): CurrentProjectStore | undefined
export function useCurrentProject(allowMissing?: false): CurrentProjectStore
export function useCurrentProject(allowMissing?: boolean): CurrentProjectStore | undefined
/**
 * A context of a single opened project.
 *
 * Use `WithCurrentProject` component to provide which project is the current for entire component
 * tree (it injects context, makes sure the project is available, and sets proper css properties).
 *
 * The refs inside aren't proxied, so this store may be deconstructed.
 */
export function useCurrentProject(allowMissing?: boolean) {
  const currentProjectStore = useCurrentProjectRaw(allowMissing)
  if (currentProjectStore == null) return undefined
  // If the store is defined, but there is no project in it, it has to be fallback component.
  if (currentProjectStore.maybeRef.value == null) {
    if (allowMissing) return undefined
    else throw new Error(`Trying to inject currentProject in WithProject's fallback component`)
  }
  return currentProjectStore
}

function useStoreTemplate<
  K extends Exclude<keyof CurrentProjectStore, 'maybeRef' | 'id' | 'ensoPath'>,
>(storeKey: K): () => NonNullable<CurrentProjectStore[K]['value']> {
  return () => {
    const currentProject = useCurrentProject().maybeRef
    const store: Opt<CurrentProjectStore[K]['value']> = currentProject.value?.[storeKey]
    if (store == null) {
      throw new Error('Current Project missing, probably closed.')
    }
    watch(currentProject, () => {
      throw new Error(
        `Component used ${storeKey} in a deprecated way, and wasn't cleaned with the project`,
      )
    })
    return store
  }
}

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useProjectStore = useStoreTemplate('store')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useProjectNames = useStoreTemplate('projectNames')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useSuggestionDbStore = useStoreTemplate('suggestionDb')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useGraphStore = useStoreTemplate('graph')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useWidgetRegistry = useStoreTemplate('widgetRegistry')
</script>

<script setup lang="ts">
const { id } = defineProps<{ id: Opt<ProjectId> }>()

const openedProjects = useOpenedProjects()
const project = computed(() => (id ? openedProjects.get(id) : undefined))
const initializedProject = computed(() =>
  project.value?.state.status === 'initialized' ? project.value.state : undefined,
)
const providedProject = shallowRef<InitializedProject | undefined>(initializedProject.value)

// When project appears, the setup and mount handlers should already see it in context. But when project disappears,
// we want to keep stores while unmounting (because unmount handlers may still read some computed values).
// That's why we use two separate watches.
watch(
  initializedProject,
  (project) => {
    if (project != null) providedProject.value = project
  },
  { flush: 'pre' },
)
watch(
  initializedProject,
  (project) => {
    if (project == null) providedProject.value = project
  },
  { flush: 'post' },
)

provideCurrentProject(providedProject)

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  const groups = initializedProject.value?.suggestionDb.groups ?? []
  for (const group of groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})
</script>

<template>
  <div class="WithCurrentProject" :style="groupColors">
    <slot v-if="project?.error != null" name="error">
      <ResultComponent
        status="error"
        title="Failed to open project"
        :subtitle="`${project.error}`"
      />
    </slot>
    <slot v-else-if="initializedProject != null" />
    <slot
      v-else-if="
        project?.nextTask?.process === 'opening' || project?.nextTask?.process === 'restoring'
      "
      name="loading"
    >
      <Loader minHeight="full" />
    </slot>
    <slot v-else name="fallback" />
  </div>
</template>

<style scoped>
.WithCurrentProject {
  display: contents;
}
</style>
