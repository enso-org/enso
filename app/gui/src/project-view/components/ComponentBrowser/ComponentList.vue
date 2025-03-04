<script setup lang="ts">
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import ComponentEntry from '@/components/ComponentBrowser/ComponentEntry.vue'
import type { Filtering } from '@/components/ComponentBrowser/filtering'
import SvgIcon from '@/components/SvgIcon.vue'
import VirtualizedList from '@/components/VirtualizedList.vue'
import { groupColorStyle } from '@/composables/nodeColors'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/data/array'
import { computed, ref, toRef, watch } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

const ITEM_SIZE = 24
const SCROLL_TO_SELECTION_MARGIN = ITEM_SIZE / 2
const MOUSE_SELECTION_DEBOUNCE = 200

const props = defineProps<{
  filtering: Filtering
}>()
const emit = defineEmits<{
  acceptSuggestion: [suggestion: Component]
  'update:selectedComponent': [selected: Component | null]
}>()

const root = ref<HTMLElement>()
const groupsPanel = ref<ComponentExposed<typeof VirtualizedList>>()
const componentsPanel = ref<ComponentExposed<typeof VirtualizedList>>()
const panels = { groupsPanel, componentsPanel }
export type ComponentListPanel = keyof typeof panels

const selectedGroupIndex = ref<number | null>(0)
const selectedComponentIndex = ref<number | null>(0)
const focusedPanel = ref<ComponentListPanel>('componentsPanel')

const displayedSelectedComponentIndex = computed({
  get: () => (focusedPanel.value === 'groupsPanel' ? null : selectedComponentIndex.value),
  set: (index) => {
    selectedComponentIndex.value = index
    if (index != null) {
      focusedPanel.value = 'componentsPanel'
    }
  },
})

watch(toRef(props, 'filtering'), () => (displayedSelectedComponentIndex.value = 0))
watch(selectedGroupIndex, () => (selectedComponentIndex.value = 0))

const suggestionDbStore = useSuggestionDbStore()
const components = computed(() => makeComponentList(suggestionDbStore.entries, props.filtering))
const currentGroups = computed(() => {
  return Array.from(components.value.entries(), ([id, components]) => ({
    id,
    ...(id === 'all' ? { name: 'all' }
    : id === 'suggestions' ? { name: 'suggestions' }
    : (suggestionDbStore.groups[id] ?? { name: 'unknown' })),
    ...(props.filtering.pattern != null ? { displayedNumber: components.length } : {}),
  }))
})
const displayedGroupId = computed(() =>
  selectedGroupIndex.value != null ? currentGroups.value[selectedGroupIndex.value]?.id : null,
)

const currentComponents = computed(() => {
  if (displayedGroupId.value == null) return components.value.get('all') ?? []
  else return components.value.get(displayedGroupId.value) ?? []
})

/** Group colors are populated in `GraphEditor`, and for each group in suggestion database a CSS variable is created. */
function componentColor(component: Component): string {
  return groupColorStyle(tryGetIndex(suggestionDbStore.groups, component.group))
}

const selectedComponent = computed(() =>
  selectedComponentIndex.value == null ?
    null
  : (currentComponents.value[selectedComponentIndex.value] ?? null),
)

watch(selectedComponent, (component) => emit('update:selectedComponent', component), {
  immediate: true,
})

defineExpose({
  switchPanelFocus: () => {
    switch (focusedPanel.value) {
      case 'componentsPanel':
        focusedPanel.value = 'groupsPanel'
        break
      case 'groupsPanel':
        focusedPanel.value = 'componentsPanel'
        // VirtualizedList component may have set selection to null on item list update.
        selectedComponentIndex.value = 0
        break
    }
  },
  moveUp: () => panels[focusedPanel.value].value?.moveUp(),
  moveDown: () => panels[focusedPanel.value].value?.moveDown(),
})
</script>

<template>
  <div ref="root" class="ComponentList">
    <VirtualizedList
      v-slot="{ item: group, selected }"
      ref="groupsPanel"
      v-model:selected="selectedGroupIndex"
      class="groups"
      :items="currentGroups"
      :itemHeight="ITEM_SIZE"
      :scrollToSelectionMargin="SCROLL_TO_SELECTION_MARGIN"
      :autoSelectFirst="true"
      :debounceMouseSelection="MOUSE_SELECTION_DEBOUNCE"
    >
      <div class="groupEntry">
        <span class="groupEntryLabel">
          {{ group.name }}{{ group.displayedNumber ? ` (${group.displayedNumber})` : '' }}
        </span>
        <SvgIcon v-if="selected" class="groupEntryIcon" name="folder_closed" />
      </div>
    </VirtualizedList>
    <VirtualizedList
      ref="componentsPanel"
      v-slot="{ item: component }"
      v-model:selected="displayedSelectedComponentIndex"
      class="components"
      :items="currentComponents"
      :itemHeight="ITEM_SIZE"
      :scrollToSelectionMargin="SCROLL_TO_SELECTION_MARGIN"
      :autoSelectFirst="focusedPanel === 'componentsPanel'"
      :debounceMouseSelection="MOUSE_SELECTION_DEBOUNCE"
      @itemAccepted="emit('acceptSuggestion', $event)"
    >
      <ComponentEntry :component="component" :color="componentColor(component)" />
    </VirtualizedList>
  </div>
</template>

<style scoped>
.ComponentList {
  width: 661px;
  height: 370px;
  border: none;
  border-radius: var(--radius-default);
  background-color: var(--background-color);
  display: flex;
  flex-direction: row;
}

.groups {
  width: 129px;
  height: 100%;
  flex-grow: 0;
  padding: 9px;
  border-radius: var(--radius-default) 0 0 var(--radius-default);
  background-color: #dadada;
}

.groupEntry {
  width: 100%;
  height: 24px;
  border-radius: 12px;
  align-content: center;
  padding: 7px;
  line-height: 1;
  font-family: var(--font-code);
  display: flex;
  flex-direction: row;
  align-items: center;

  &.selected {
    background-color: white;
  }
}

.groupEntryLabel {
  flex-grow: 1;
}

.groupEntryIcon {
  --icon-size: 12px;
}

.components {
  flex-grow: 1;
  padding: 9px;
}
</style>
