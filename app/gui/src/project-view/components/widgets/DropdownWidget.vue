<script setup lang="ts" generic="Entry extends DropdownEntry">
import SvgIcon from '@/components/SvgIcon.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
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
const graphNavigator = injectGraphNavigator(true)

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

const styleVars = computed(() => {
  return {
    '--dropdown-bg': props.color,
    // Slightly shift the top border of drawn dropdown away from node's top border by a fraction of
    // a pixel, to prevent it from poking through and disturbing node's siluette.
    '--extend-margin': `${0.2 / (graphNavigator?.scale ?? 1)}px`,
  }
})
</script>

<script lang="ts">
export interface DropdownEntry {
  readonly value: string
  readonly selected: boolean
}
</script>

<template>
  <div class="DropdownWidget" :style="styleVars">
    <ul class="list scrollable" @wheel.stop.passive>
      <li
        v-for="entry in sortedValues"
        :key="entry.value"
        :class="{ selected: entry.selected }"
        class="item clickable"
        @click.stop="emit('clickEntry', entry, $event.altKey)"
      >
        <div class="itemContent" v-text="entry.value"></div>
      </li>
    </ul>
    <div v-if="enableSortButton" class="sort">
      <div class="sort-background"></div>
      <SvgIcon
        :name="ICON_LOOKUP[sortDirection]"
        class="clickable"
        @click="sortDirection = NEXT_SORT_DIRECTION[sortDirection]"
      />
    </div>
  </div>
</template>

<style scoped>
.DropdownWidget {
  --item-height: 23px;
  --visible-items: 6.4;
  --dropdown-padding: 6px;
  --item-padding: 8px;
  /* When dropdown is displayed right below the last node's argument, the rounded corner needs to be
     covered. This is done by covering extra node-sized space at the top of the dropdown. */
  --dropdown-extend: calc(var(--node-base-height) - var(--extend-margin));

  position: relative;
  user-select: none;
  min-width: 100%;
  margin-top: calc(0px - var(--dropdown-extend));
  padding-top: var(--dropdown-extend);
  background-color: var(--dropdown-bg);
  border-radius: calc(var(--item-height) / 2 + var(--dropdown-padding));

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
  min-width: 100%;
  min-height: 16px;
  max-height: calc(var(--visible-items) * var(--item-height) + 2 * var(--dropdown-padding));
  list-style-type: none;
  color: var(--color-text-light);
  scrollbar-width: thin;
  padding: var(--dropdown-padding);
  position: relative;
}

.item {
  padding-left: var(--item-padding);
  padding-right: var(--item-padding);
  border-radius: calc(var(--item-height) / 2);
  height: var(--item-height);
  text-align: left;
  max-width: 100%;
  overflow: hidden;

  &:hover {
    background-color: color-mix(in oklab, var(--color-port-connected) 50%, transparent 50%);
    span {
      --text-scroll-max: calc(var(--dropdown-max-width) - 28px);
      will-change: transform;
      animation: 6s 1s infinite text-scroll;
    }
  }

  &:not(.selected):hover {
    color: white;
  }

  &.selected {
    background-color: var(--color-port-connected);

    & + .selected {
      border-top-left-radius: 0;
      border-top-right-radius: 0;
    }

    &:has(+ .selected) {
      border-bottom-left-radius: 0;
      border-bottom-right-radius: 0;
    }
  }
}

.itemContent {
  display: inline-block;
  max-width: 100%;
  white-space: nowrap;
  overflow: hidden;
  vertical-align: middle;
  margin: 3px 0;
  text-wrap: nowrap;
  text-overflow: ellipsis;
}

@keyframes text-scroll {
  0%,
  80%,
  100% {
    max-width: unset;
    transform: translateX(0);
  }
  50%,
  70% {
    max-width: unset;
    transform: translateX(calc(min(var(--text-scroll-max, 100%), 100%) - 100%));
  }
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
</style>
