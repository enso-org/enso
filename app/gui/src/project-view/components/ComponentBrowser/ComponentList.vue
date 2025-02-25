<script setup lang="ts">
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import LazyList from '@/components/LazyList.vue'

import { groupColorStyle } from '@/composables/nodeColors'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/data/array'
import { allRanges } from '@/util/data/range'
import { computed } from 'vue'
import ComponentEntry from './ComponentEntry.vue'

const ITEM_SIZE = 36

const props = defineProps<{
  filtering: Filtering
  autoSelectFirstComponent: boolean
}>()
const emit = defineEmits<{
  acceptSuggestion: [suggestion: Component]
  'update:selectedComponent': [selected: Component | null]
  'update:selectedGroup': [selected: number | null]
}>()

const suggestionDbStore = useSuggestionDbStore()
const components = computed(() => makeComponentList(suggestionDbStore.entries, props.filtering))
/** Group colors are populated in `GraphEditor`, and for each group in suggestion database a CSS variable is created. */
function componentColor(component: Component): string {
  return groupColorStyle(tryGetIndex(suggestionDbStore.groups, component.group))
}
</script>

<template>
  <div class="ComponentList">
    <LazyList
      v-slot="{ item: group }"
      class="groups"
      :items="suggestionDbStore.groups"
      :itemHeight="ITEM_SIZE"
      :autoSelectFirst="true"
      @update:selectedItem="(_, index) => emit('update:selectedGroup', index)"
    >
      <div class="groupEntry">{{ group.name }}</div>
    </LazyList>
    <LazyList
      class="components"
      :items="components"
      :itemHeight="ITEM_SIZE"
      :autoSelectFirst="autoSelectFirstComponent"
      @itemAccepted="emit('acceptSuggestion', $event)"
      @update:selectedItem="emit('update:selectedComponent', $event)"
    >
      <template #default="{ item: component }">
        <ComponentEntry :component="component" :color="componentColor(component)" />
      </template>
      <template #selected="{ item: component }">
        <ComponentEntry
          class="selected"
          :component="component"
          :color="componentColor(component)"
        />
      </template>
    </LazyList>
  </div>
</template>

<style scoped>
.ComponentList {
  width: 661px;
  height: 380px;
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
  height: 36px;
  align-content: center;
  padding: 9px;
  line-height: 1;
  font-family: var(--font-code);

  &.selected {
    background-color: white;
  }
}

.components {
  flex-grow: 1;
  padding: 13px;
}
</style>
