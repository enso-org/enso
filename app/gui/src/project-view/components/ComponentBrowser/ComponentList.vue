<script setup lang="ts">
import { GroupId, makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import ComponentEntry from '@/components/ComponentBrowser/ComponentEntry.vue'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import LazyList from '@/components/LazyList.vue'
import { groupColorStyle } from '@/composables/nodeColors'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/data/array'
import { computed, ref, type ComponentInstance } from 'vue'

const ITEM_SIZE = 24
const SCROLL_TO_SELECTION_MARGIN = ITEM_SIZE / 2

const props = defineProps<{
  filtering: Filtering
  autoSelectFirstComponent: boolean
  focusedPanel: ComponentListPanel
}>()
const emit = defineEmits<{
  acceptSuggestion: [suggestion: Component]
  'update:selectedComponent': [selected: Component | null]
}>()

const groupsPanel = ref<ComponentInstance<typeof LazyList>>()
const componentsPanel = ref<ComponentInstance<typeof LazyList>>()
const panels = { groupsPanel, componentsPanel }
export type ComponentListPanel = keyof typeof panels

const selectedGroup = ref<GroupId | null>(null)
const suggestionDbStore = useSuggestionDbStore()
const components = computed(() => makeComponentList(suggestionDbStore.entries, props.filtering))
const currentComponents = computed(() => {
  if (selectedGroup.value == null) return components.value.get('all') ?? []
  else return components.value.get(selectedGroup.value) ?? []
})
const currentGroups = computed(() => {
  return Array.from(components.value.keys(), (id) => ({
    id,
    ...(id === 'all' ? { name: 'all' }
    : id === 'suggestions' ? { name: 'suggestions' }
    : (suggestionDbStore.groups[id] ?? { name: 'unknown' })),
  }))
})
/** Group colors are populated in `GraphEditor`, and for each group in suggestion database a CSS variable is created. */
function componentColor(component: Component): string {
  return groupColorStyle(tryGetIndex(suggestionDbStore.groups, component.group))
}

defineExpose({
  moveUp: () => {
    panels[props.focusedPanel].value?.moveUp()
  },
  moveDown: () => {
    panels[props.focusedPanel].value?.moveDown()
  },
})
</script>

<template>
  <div class="ComponentList">
    <LazyList
      v-slot="{ item: group }"
      ref="groupsPanel"
      class="groups"
      :items="currentGroups"
      :itemHeight="ITEM_SIZE"
      :scrollToSelectionMargin="SCROLL_TO_SELECTION_MARGIN"
      :autoSelectFirst="true"
      @update:selectedItem="(group) => (selectedGroup = group?.id ?? null)"
    >
      <div class="groupEntry">{{ group.name }}</div>
    </LazyList>
    <LazyList
      ref="componentsPanel"
      v-slot="{ item: component }"
      class="components"
      :items="currentComponents"
      :itemHeight="ITEM_SIZE"
      :scrollToSelectionMargin="SCROLL_TO_SELECTION_MARGIN"
      :autoSelectFirst="autoSelectFirstComponent"
      @itemAccepted="emit('acceptSuggestion', $event)"
      @update:selectedItem="emit('update:selectedComponent', $event)"
    >
      <ComponentEntry :component="component" :color="componentColor(component)" />
    </LazyList>
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

  &.selected {
    background-color: white;
  }
}

.components {
  flex-grow: 1;
  padding: 9px;
}
</style>
