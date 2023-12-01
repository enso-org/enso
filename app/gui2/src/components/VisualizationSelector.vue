<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useAutoBlur } from '@/util/autoBlur'
import { visIdentifierEquals, type VisualizationIdentifier } from 'shared/yjsModel'
import { onMounted, ref } from 'vue'

const props = defineProps<{
  types: Iterable<VisualizationIdentifier>
  modelValue: VisualizationIdentifier
}>()
const emit = defineEmits<{ hide: []; 'update:modelValue': [type: VisualizationIdentifier] }>()

// This dynamic import is required to break the circular import:
// `VisualizationSelector.vue` -> `compilerMessaging.ts` -> `VisualizationContainer.vue` ->
// `VisualizationSelector.vue`
const visualizationStore = (await import('@/stores/visualization')).useVisualizationStore()

const rootNode = ref<HTMLElement>()
useAutoBlur(rootNode)

function visIdLabel(id: VisualizationIdentifier) {
  switch (id.module.kind) {
    case 'Builtin':
      return id.name
    case 'Library':
      return `${id.name} (from library ${id.module.name})`
    case 'CurrentProject':
      return `${id.name} (from project)`
  }
}

function visIdKey(id: VisualizationIdentifier) {
  const kindKey = id.module.kind === 'Library' ? `Library::${id.module.name}` : id.module.kind
  return `${kindKey}::${id.name}`
}

onMounted(() => setTimeout(() => rootNode.value?.querySelector('button')?.focus(), 1))
</script>

<template>
  <div
    ref="rootNode"
    class="VisualizationSelector"
    @focusout="$event.relatedTarget == null && emit('hide')"
  >
    <div class="background"></div>
    <ul>
      <li
        v-for="type_ in props.types"
        :key="visIdKey(type_)"
        :class="{ selected: visIdentifierEquals(props.modelValue, type_) }"
        @pointerdown.stop="emit('update:modelValue', type_)"
      >
        <button>
          <SvgIcon class="icon" :name="visualizationStore.icon(type_) ?? 'columns_increasing'" />
          <span v-text="visIdLabel(type_)"></span>
        </button>
      </li>
    </ul>
  </div>
</template>

<style scoped>
.VisualizationSelector {
  /* Required for it to show above Mapbox's information button. */
  z-index: 2;
  user-select: none;
  position: absolute;
  border-radius: 16px;
  top: 100%;
  margin-top: 12px;
  left: -12px;

  &:before {
    content: '';
    position: absolute;
    width: 100%;
    height: 100%;
    border-radius: 16px;
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
}

.VisualizationSelector > * {
  position: relative;
}

ul {
  display: flex;
  flex-flow: column;
  gap: 2px;
  list-style-type: none;
  padding: 4px;
}

button {
  width: 100%;
  display: flex;
  gap: 4px;
  align-items: center;
  cursor: pointer;
  padding: 0 8px;
  border-radius: 12px;
  white-space: nowrap;

  &.selected {
    background: var(--color-menu-entry-selected-bg);
  }

  &:hover {
    background: var(--color-menu-entry-hover-bg);
  }
}
</style>
