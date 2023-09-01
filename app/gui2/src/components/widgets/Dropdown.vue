<script setup lang="ts">
import SortAscendingIcon from '@/assets/icons/sort_ascending.svg'
import SortDescendingIcon from '@/assets/icons/sort_descending.svg'
import SortIcon from '@/assets/icons/sort.svg'
import { computed, ref } from 'vue'

enum SortDirection {
  none = 'none',
  ascending = 'ascending',
  descending = 'descending',
}

const props = defineProps<{ color: string; selectedValue: string | null; values: string[] }>()
const emit = defineEmits<{ click: [index: number] }>()

const sortDirection = ref<SortDirection>(SortDirection.none)

const sortedValuesAndIndices = computed(() => {
  const valuesAndIndices = props.values.map<[value: string, index: number]>(
    (value, index) => [value, index]
  )
  switch (sortDirection.value) {
    case SortDirection.none: {
      return valuesAndIndices
    }
    case SortDirection.ascending: {
      return valuesAndIndices.sort((a, b) => a[0] > b[0] ? 1 : a[0] < b[0] ? -1 : 0)
    }
    case SortDirection.descending: {
      return valuesAndIndices.sort((a, b) => a[0] > b[0] ? -1 : a[0] < b[0] ? 1 : 0)
    }
  }
})

const ICON_LOOKUP: Record<SortDirection, string> = {
  [SortDirection.none]: SortIcon,
  [SortDirection.ascending]: SortAscendingIcon,
  [SortDirection.descending]: SortDescendingIcon,
}

const NEXT_SORT_DIRECTION: Record<SortDirection, SortDirection> = {
  [SortDirection.none]: SortDirection.ascending,
  [SortDirection.ascending]: SortDirection.descending,
  [SortDirection.descending]: SortDirection.none,
}
</script>

<template>
  <div :tabindex="-1" class="DropdownContainer" @wheel.stop>
    <div class="Dropdown" :style="{ background: color }">
      <template v-for="[value, index] in sortedValuesAndIndices">
        <div v-if="value === selectedValue">
          <div class="selected-item"><span v-text="value"></span></div>
        </div>
        <div v-else class="selectable-item button" @click="emit('click', index)"><span v-text="value"></span></div>
      </template>
      <img class="sort button" :src="ICON_LOOKUP[sortDirection]"
        @click="sortDirection = NEXT_SORT_DIRECTION[sortDirection]">
    </div>
  </div>
</template>

<style scoped>
.DropdownContainer {
  user-select: none;
  overflow: auto;
  clip-path: inset(0 round 8px);
  width: min-content;
  height: 136px;
  -ms-overflow-style: none;
  scrollbar-width: none;
}

.DropdownContainer::-webkit-scrollbar {
  display: none;
}

.Dropdown {
  position: relative;
  color: var(--color-text-light);
  padding-left: 8px;
  padding-right: 8px;
  width: min-content;
}

.Dropdown>* {
  position: relative;
}

.sort {
  position: absolute;
  top: 3px;
  right: 3px;
}

.selected-item {
  border-radius: var(--radius-full);
  background-color: var(--color-port-connected);
  padding-left: 8px;
  padding-right: 8px;
}
</style>
