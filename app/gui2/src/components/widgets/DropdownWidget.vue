<script setup lang="ts" generic="Entry extends DropdownEntry">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed, ref } from 'vue'

enum SortDirection {
  none = 'none',
  ascending = 'ascending',
  descending = 'descending',
}

const props = defineProps<{ color: string; entries: Entry[] }>()
const emit = defineEmits<{ click: [entry: Entry, keepOpen: boolean] }>()

const sortDirection = ref<SortDirection>(SortDirection.none)

function lexicalCmp(a: string, b: string) {
  return (
    a > b ? 1
    : a < b ? -1
    : 0
  )
}

const sortedValues = computed<Entry[]>(() => {
  switch (sortDirection.value) {
    case SortDirection.ascending: {
      return [...props.entries].sort((a, b) => lexicalCmp(a.value, b.value))
    }
    case SortDirection.descending: {
      return [...props.entries].sort((a, b) => lexicalCmp(b.value, a.value))
    }
    case SortDirection.none:
    default: {
      return props.entries
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

// Currently unused.
const enableSortButton = ref(false)
</script>

<script lang="ts">
export interface DropdownEntry {
  readonly value: string
  readonly selected: boolean
}
</script>

<template>
  <div class="Dropdown" @pointerdown.stop @pointerup.stop @click.stop>
    <ul class="list scrollable" :style="{ background: color, borderColor: color }" @wheel.stop>
      <template v-for="entry in sortedValues" :key="entry.value">
        <li v-if="entry.selected">
          <div class="item selected button" @click.stop="emit('click', entry, $event.altKey)">
            <span v-text="entry.value"></span>
          </div>
        </li>
        <li v-else class="item button" @click.stop="emit('click', entry, $event.altKey)">
          <span v-text="entry.value"></span>
        </li>
      </template>
    </ul>
    <div v-if="enableSortButton" class="sort button">
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
  margin-top: 8px;
  overflow: visible;
}

.list {
  position: relative;
  user-select: none;
  overflow: auto;
  border-radius: 16px;
  width: min-content;
  max-height: 152px;
  list-style-type: none;
  color: var(--color-text-light);
  scrollbar-width: thin;
  padding: 4px 0;
  border: 2px solid;
}

li {
  text-align: left;
}

.item:not(.selected):hover {
  color: white;
}

.list span {
  display: inline-block;
  vertical-align: middle;
  margin: 3px 0;
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
  padding: 2px 0 2px 2px;
  line-height: 0;
}

.sort > img {
  position: relative;
}

.item {
  margin-right: 8px;
  padding-left: 8px;
  padding-right: 8px;
}

.item.selected {
  border-radius: var(--radius-full);
  background-color: var(--color-port-connected);
  width: min-content;
}
</style>
