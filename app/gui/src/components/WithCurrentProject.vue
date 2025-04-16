<script lang="ts">
import { injectOpenedProjects } from '$/providers/openedProjects'
import { groupColorVar } from '@/composables/nodeColors'
import { createContextStore } from '@/providers'
import { colorFromString } from '@/util/colors'
import { Opt } from '@/util/data/opt'
import { ToValue } from '@/util/reactivity'
import { computed, toValue, watch } from 'vue'

export type CurrentProject = ReturnType<typeof injectCurrentProject>
const [provideCurrentProject, injectCurrentProject] = createContextStore(
  'currentProject',
  (projectId: ToValue<Opt<string>>) => {
    const openedProjects = injectOpenedProjects()

    const currentProject = computed(() => {
      const id = toValue(projectId)
      return id != null ? openedProjects.get(id) : undefined
    })

    return {
      id: computed(() => (currentProject.value ? projectId : undefined)),
      store: computed(() => currentProject.value?.store),
      names: computed(() => currentProject.value?.names),
      suggestionDb: computed(() => currentProject.value?.suggestionDb),
      graph: computed(() => currentProject.value?.graph),
      widgetRegistry: computed(() => currentProject.value?.widgetRegistry),
    }
  },
)

export { injectCurrentProject }

function useStoreTemplate<K extends keyof CurrentProject>(
  storeKey: K,
): () => NonNullable<CurrentProject[K]['value']> {
  return () => {
    const currentProject = injectCurrentProject()
    const store: CurrentProject[K]['value'] = currentProject[storeKey].value
    if (store == null) {
      throw new Error('Current Project missing, probably closed.')
    }
    watch(currentProject[storeKey], () => {
      throw new Error(
        `Component used ${storeKey} without argument and wasn't cleaned with the project`,
      )
    })
    return store
  }
}

/** @deprecated use injectCurrentProject */
export const useProjectStore = useStoreTemplate('store')

/** @deprecated use injectCurrentProject */
export const useProjectNames = useStoreTemplate('names')

/** @deprecated use injectCurrentProject */
export const useSuggestionDbStore = useStoreTemplate('suggestionDb')

/** @deprecated use injectCurrentProject */
export const useGraphStore = useStoreTemplate('graph')

/** @deprecated use injectCurrentProject */
export const useWidgetRegistry = useStoreTemplate('widgetRegistry')
</script>

<script setup lang="ts">
const { id, onlyDefined = false } = defineProps<{ id: Opt<string>; onlyDefined?: boolean }>()

const { id: providedId, suggestionDb } = provideCurrentProject(() => id)

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  const groups = suggestionDb.value?.groups ?? []
  for (const group of groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})
</script>

<template>
  <div class="WithCurrentProject" :style="groupColors">
    <slot v-if="!onlyDefined || providedId != null" />
  </div>
</template>

<style scoped>
.WithCurrentProject {
  display: contents;
}
</style>
