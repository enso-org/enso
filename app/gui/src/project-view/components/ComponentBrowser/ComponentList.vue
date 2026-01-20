<script setup lang="ts">
import {
  useProjectNames,
  useProjectStore,
  useSuggestionDbStore,
} from '$/components/WithCurrentProject.vue'
import { makeComponentLists, type Component } from '@/components/ComponentBrowser/component'
import ComponentEntry from '@/components/ComponentBrowser/ComponentEntry.vue'
import { Filtering, type Filter } from '@/components/ComponentBrowser/filtering'
import SvgIcon from '@/components/SvgIcon.vue'
import VirtualizedList from '@/components/VirtualizedList.vue'
import { groupColorStyle } from '@/composables/nodeColors'
import { Ast } from '@/util/ast'
import { substituteQualifiedName } from '@/util/ast/abstract'
import { tryGetIndex } from '@/util/data/array'
import { qnLastSegment } from '@/util/qualifiedName'
import * as map from 'lib0/map'
import { computed, ref, watch } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { parseExpression } from 'ydoc-shared/ast'
import ActionButton from '../ActionButton.vue'

const ITEM_SIZE = 24
const SCROLL_TO_SELECTION_MARGIN = ITEM_SIZE / 2
const MOUSE_SELECTION_DEBOUNCE = 200

const props = defineProps<{
  filter: Filter
  literal?: Ast.Ast | undefined
}>()
const emit = defineEmits<{
  acceptSuggestion: [suggestion: Component]
  'update:selectedComponent': [selected: Component | null]
}>()

const projectStore = useProjectStore()
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

const filtering = computed(() => {
  const currentModule = projectStore.moduleProjectPath
  return new Filtering(props.filter, currentModule?.ok ? currentModule.value : undefined)
})

watch(filtering, () => (displayedSelectedComponentIndex.value = 0))
watch(selectedGroupIndex, () => (selectedComponentIndex.value = 0))

const suggestionDbStore = useSuggestionDbStore()
const projectNames = useProjectNames()
const components = computed(() => {
  const lists = makeComponentLists(suggestionDbStore.entries, filtering.value)
  if (props.literal != null) {
    map
      .setIfUndefined(lists, 'all', (): Component[] => [])
      .unshift({
        label: props.literal.code(),
        icon: props.literal instanceof Ast.TextLiteral ? 'text_input' : 'input_number',
      })
  }
  return lists
})
const currentGroups = computed(() => {
  return Array.from(components.value.entries(), ([id, components]) => ({
    id,
    ...(id === 'all' ? { name: 'all' }
    : id === 'suggestions' ? { name: 'suggestions' }
    : (suggestionDbStore.groups[id] ?? { name: 'unknown' })),
    ...(filtering.value?.pattern != null ? { displayedNumber: components.length } : {}),
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

const selectedSuggestion = computed(() => {
  if (selectedComponent.value?.suggestionId == null) return null
  return suggestionDbStore.entries.get(selectedComponent.value.suggestionId)
})
const documentationSummary = computed(() => selectedSuggestion.value?.documentationSummary)

const selectedSuggestionReturnType = computed(() => {
  if (selectedSuggestion.value == null) return undefined
  const typename = selectedSuggestion.value.returnType(projectNames)

  const parsedType = parseExpression(typename)
  if (parsedType == null) return typename
  const substituted = substituteQualifiedName(parsedType, (qn) => qnLastSegment(qn))
  return substituted.code()
})

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
    <div class="rightPane">
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
      <div class="documentation">
        <div class="documentationContent">
          <!-- eslint-disable-next-line vue/no-v-html -->
          <p v-if="documentationSummary" v-html="documentationSummary" />
          <p v-if="selectedSuggestion" v-text="`Returns: ${selectedSuggestionReturnType}`" />
        </div>
        <ActionButton class="helpButton" action="graphEditor.showHelp" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.ComponentList {
  width: 661px;
  height: 386px;
  border: none;
  border-radius: var(--radius-default);
  background-color: var(--background-color);
  display: flex;
  flex-direction: row;
}

.groups {
  width: 129px;
  min-width: 129px;
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

.rightPane {
  flex-grow: 1;
  padding: 9px;
  display: flex;
  flex-direction: column;
  gap: 9px;
  min-width: 0;
}

.components {
  flex-grow: 1;
}

.documentation {
  border-top: 1px solid #d9d9d9;
  padding-top: 9px;
  display: flex;
  flex-direction: row;
  width: 100%;
}

.documentationContent {
  min-width: 0;
  flex-grow: 1;
  p {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    /* If help contains <code> tags, it is a bit higher, resulting in panel hight jump. */
    height: 23px;
  }
}

.helpButton {
  width: 24px;
  height: 24px;
}
</style>
