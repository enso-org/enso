<script lang="ts">
import { ProjectId } from '#/services/Backend'
import { isLocalProjectId } from '#/services/LocalBackend'
import { injectOpenedProjects, type OpenedProject } from '$/providers/openedProjects'
import { groupColorVar } from '@/composables/nodeColors'
import { createContextStore } from '@/providers'
import { colorFromString } from '@/util/colors'
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, watch, type ToRefs } from 'vue'

/**
 * A context of a single opened project.
 *
 * Use `WithCurrentProject` component to provide which project is the current for entire component
 * tree (it's injects context and also sets proper css properties). Inside, inject will bring all
 * project-related stores. If the project is closed, all stores becomes undefined.
 */
const [provideCurrentProject, useCurrentProject] = createContextStore(
  'currentProject',
  (projectId: ToValue<Opt<ProjectId>>) => {
    const openedProjects = injectOpenedProjects()

    const hybridResolvedProjectId = computed(() => {
      const id = toValue(projectId)
      // When we have a hybrid project opened, we have to translate cloud project ID to corresponding hybrid project.
      if (id && openedProjects.get(id) == null && !isLocalProjectId(id)) {
        for (const openedId of openedProjects.listIds()) {
          if (openedId.includes('/cloud-' + id) && isLocalProjectId(openedId)) return openedId
        }
      }
      return id
    })

    const ref = computed((): OpenedProject | undefined => {
      const id = hybridResolvedProjectId.value
      return id != null ? openedProjects.get(id) : undefined
    })

    return {
      id: hybridResolvedProjectId,
      /* Current project as a single ref  */
      ref,
      /* Current project's stores decomposed to separate refs. */
      storesRefs: {
        store: computed(() => ref.value?.store),
        names: computed(() => ref.value?.names),
        suggestionDb: computed(() => ref.value?.suggestionDb),
        graph: computed(() => ref.value?.graph),
        widgetRegistry: computed(() => ref.value?.widgetRegistry),
      } satisfies ToRefs<{ [K in keyof OpenedProject]: OpenedProject[K] | undefined }>,
    }
  },
)

export { useCurrentProject }

function useStoreTemplate<K extends keyof OpenedProject>(
  storeKey: K,
): () => NonNullable<OpenedProject[K]> {
  return () => {
    const currentProject = useCurrentProject().ref
    const store: Opt<OpenedProject[K]> = currentProject.value?.[storeKey]
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
export const useProjectNames = useStoreTemplate('names')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useSuggestionDbStore = useStoreTemplate('suggestionDb')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useGraphStore = useStoreTemplate('graph')

/** @deprecated it expects the current project will not change. Use {@link useCurrentProject} instead. */
export const useWidgetRegistry = useStoreTemplate('widgetRegistry')
</script>

<script setup lang="ts">
const { id } = defineProps<{ id: Opt<ProjectId> }>()

const provided = provideCurrentProject(() => id).ref

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  const groups = provided.value?.suggestionDb.groups ?? []
  for (const group of groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})
</script>

<template>
  <div class="WithCurrentProject" :style="groupColors">
    <slot />
  </div>
</template>

<style scoped>
.WithCurrentProject {
  display: contents;
}
</style>
