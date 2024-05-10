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
const emit = defineEmits<{ clickEntry: [entry: Entry, keepOpen: boolean] }>()

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
  <div class="DropdownWidget" :style="{ '--dropdown-bg': color }">
    <ul class="list scrollable" @wheel.stop>
      <template v-for="entry in sortedValues" :key="entry.value">
        <li v-if="entry.selected">
          <div class="item selected button" @click.stop="emit('clickEntry', entry, $event.altKey)">
            <span v-text="entry.value"></span>
          </div>
        </li>
        <li v-else class="item button" @click.stop="emit('clickEntry', entry, $event.altKey)">
          <span v-text="entry.value"></span>
        </li>
      </template>
    </ul>
    <div v-if="enableSortButton" class="sort button">
      <div class="sort-background"></div>
      <SvgIcon
        :name="ICON_LOOKUP[sortDirection]"
        @click="sortDirection = NEXT_SORT_DIRECTION[sortDirection]"
      />
    </div>
  </div>
</template>

<style scoped>
.DropdownWidget {
  position: relative;
  user-select: none;
  overflow: clip;
  min-width: 100%;

  /* When dropdown is displayed right below the last node's argument, the rounded corner needs to be
     covered. This is done by covering extra node-sized space at the top of the dropdown. */
  --dropdown-extend: calc(var(--node-height) - 1px);
  margin-top: calc(0px - var(--dropdown-extend));
  padding-top: var(--dropdown-extend);
  background-color: var(--dropdown-bg);
  border-radius: 16px;

  &:before {
    content: '';
    display: block;
    position: absolute;
    top: var(--dropdown-extend);
    left: 4px;
    right: 4px;
    border-top: 1px solid rgb(0 0 0 / 0.12);
    z-index: 1;
  }
}
.list {
  overflow: auto;
  width: min-content;
  border-radius: 0 0 16px 16px;
  min-width: 100%;
  max-height: 152px;
  list-style-type: none;
  color: var(--color-text-light);
  background: var(--dropdown-bg);
  scrollbar-width: thin;
  padding: 4px 0;
  border: 2px solid var(--dropdown-bg);
  position: relative;
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
  background: var(--dropdown-bg);
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
  margin-right: 4px;
  margin-left: 4px;
  padding-left: 8px;
  padding-right: 8px;
  border-radius: var(--radius-full);

  &:hover {
    background-color: color-mix(in oklab, var(--color-port-connected) 50%, transparent 50%);
  }
  &.selected {
    background-color: var(--color-port-connected);
  }
}
</style>
