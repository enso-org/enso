import type SelectionDropdown from '@/components/SelectionDropdown.vue'
import { useVisualizationStore } from '@/stores/visualization'
import type { ToValue } from '@/util/reactivity'
import { bindModelValue } from '@/util/vueDom'
import { computed, type Ref, toValue } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

interface VisualizationSelectorOptions {
  selectedType: Ref<VisualizationIdentifier>
  types: ToValue<ReadonlyArray<VisualizationIdentifier>>
}

function visLabel(id: VisualizationIdentifier) {
  switch (id.module.kind) {
    case 'Builtin':
      return id.name
    case 'Library':
      return `${id.name} (from library ${id.module.name})`
    case 'CurrentProject':
      return `${id.name} (from project)`
  }
}

function visKey(id: VisualizationIdentifier) {
  const kindKey = id.module.kind === 'Library' ? `Library::${id.module.name}` : id.module.kind
  return `${kindKey}::${id.name}`
}

/** Manages the dropdown options for selecting a visualization type. */
export function useVisualizationSelector({ selectedType, types }: VisualizationSelectorOptions) {
  const visualizationStore = useVisualizationStore()

  function visualizationByKey(value: string): VisualizationIdentifier | undefined {
    return toValue(types).find((vis) => visKey(vis) === value)
  }

  const visualizationOptions = computed(() =>
    Object.fromEntries(
      toValue(types).map((vis) => [
        visKey(vis),
        {
          icon: visualizationStore.icon(vis) ?? 'columns_increasing',
          label: visLabel(vis),
        },
      ]),
    ),
  )

  const selectedTypeKey = computed({
    get: () => visKey(selectedType.value),
    set: (value) => (selectedType.value = visualizationByKey(value)!),
  })

  return computed(
    (): ComponentProps<typeof SelectionDropdown> => ({
      ...bindModelValue(selectedTypeKey),
      options: visualizationOptions.value,
      title: 'Visualization Selector',
      entriesTestId: 'visualization-selector-entries',
    }),
  )
}
