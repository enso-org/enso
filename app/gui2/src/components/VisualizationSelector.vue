<script setup lang="ts">
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import { useVisualizationStore } from '@/stores/visualization'
import { computed } from 'vue'
import { type VisualizationIdentifier } from 'ydoc-shared/yjsModel'

const modelValue = defineModel<VisualizationIdentifier>({ required: true })
const props = defineProps<{
  types: Iterable<VisualizationIdentifier>
}>()

const visualizationStore = useVisualizationStore()

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

const visualizationByKey = computed(() => {
  const visualizations = new Map<string, VisualizationIdentifier>()
  for (const type_ of props.types) visualizations.set(visKey(type_), type_)
  return visualizations
})

const visualizationOptions = computed(() =>
  Object.fromEntries(
    Array.from(props.types, (vis) => [
      visKey(vis),
      {
        icon: visualizationStore.icon(vis) ?? 'columns_increasing',
        label: visLabel(vis),
      },
    ]),
  ),
)
</script>

<template>
  <SelectionDropdown
    :modelValue="visKey(modelValue)"
    :options="visualizationOptions"
    title="Visualization Selector"
    class="VisualizationSelector"
    alwaysShowArrow
    @update:modelValue="modelValue = visualizationByKey.get($event as string)!"
  />
</template>
