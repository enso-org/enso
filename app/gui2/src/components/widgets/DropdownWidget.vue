<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed, ref } from 'vue'

enum SortDirection {
  none = 'none',
  ascending = 'ascending',
  descending = 'descending',
}

const props = defineProps<{ color: string; selectedValue: string | undefined; values: string[] }>()
const emit = defineEmits<{ click: [index: number] }>()

const sortDirection = ref<SortDirection>(SortDirection.none)

const sortedValuesAndIndices = computed(() => {
  const valuesAndIndices = props.values.map<[value: string, index: number]>((value, index) => [
    value,
    index,
  ])
  switch (sortDirection.value) {
    case SortDirection.ascending: {
      return valuesAndIndices.sort((a, b) => (a[0] > b[0] ? 1 : a[0] < b[0] ? -1 : 0))
    }
    case SortDirection.descending: {
      return valuesAndIndices.sort((a, b) => (a[0] > b[0] ? -1 : a[0] < b[0] ? 1 : 0))
    }
    case SortDirection.none:
    default: {
      return valuesAndIndices
    }
  }
})

const ICON_LOOKUP: Record<SortDirection, Icon> = {
  [SortDirection.none]: 'sort',
  [SortDirection.ascending]: 'sort_ascending',
  [SortDirection.descending]: 'sort_descending',
}

const NEXT_SORT_DIRECTION: Record<SortDirection, SortDirection> = {
  [SortDirection.none]: SortDirection.ascending,
  [SortDirection.ascending]: SortDirection.descending,
  [SortDirection.descending]: SortDirection.none,
}
</script>

<template>
  <div class="Dropdown">
    <ul class="list" :style="{ background: color }" @wheel.stop>
      <template v-for="[value, index] in sortedValuesAndIndices" :key="value">
        <li v-if="value === selectedValue">
          <div class="selected-item"><span v-text="value"></span></div>
        </li>
        <li v-else class="selectable-item button" @pointerdown="emit('click', index)">
          <span v-text="value"></span>
        </li>
      </template>
    </ul>
    <div class="sort button">
      <div class="sort-background" :style="{ background: color }"></div>
      <SvgIcon
        :name="ICON_LOOKUP[sortDirection]"
        @click="sortDirection = NEXT_SORT_DIRECTION[sortDirection]"
      />
    </div>
  </div>
</template>

<style scoped>
.Dropdown {
  position: absolute;
  top: 100%;
  margin-top: 4px;
  height: 136px;
}

.list {
  position: relative;
  user-select: none;
  overflow: auto;
  border-radius: 8px;
  width: min-content;
  height: 100%;
  scrollbar-width: none;
  scrollbar-gutter: stable both-edges;
  list-style-type: none;
  color: var(--color-text-light);
  padding: 4px 0;
}

.list::-webkit-scrollbar {
  -webkit-appearance: none;
  width: 8px;
}

.list::-webkit-scrollbar-track {
  -webkit-box-shadow: none;
}

.list::-webkit-scrollbar-thumb {
  border: 2px solid #0000;
  border-left-width: 1px;
  border-right-width: 3px;
  background-clip: padding-box;
  border-radius: var(--radius-full);
  background-color: rgba(0, 0, 0, 0.2);
}

.list::-webkit-scrollbar-corner {
  background: rgba(0, 0, 0, 0);
}

.list::-webkit-scrollbar-button {
  height: 4px;
}

.sort-background {
  position: absolute;
  border-top-left-radius: var(--radius-full);
  border-bottom-left-radius: var(--radius-full);
  opacity: 0.5;
  left: 0;
  top: 0;
  height: 100%;
  width: 100%;
}

.sort {
  position: absolute;
  border-top-left-radius: var(--radius-full);
  border-bottom-left-radius: var(--radius-full);
  top: 1px;
  right: 6px;
  padding: 2px;
  padding-right: 0;
  line-height: 0;
}

.sort > img {
  position: relative;
}

.selected-item {
  border-radius: var(--radius-full);
  background-color: var(--color-port-connected);
  padding-left: 8px;
  padding-right: 8px;
  width: min-content;
}

.selectable-item {
  margin-right: 16px;
}
</style>
