<script setup lang="ts" generic="Entry extends DropdownEntry">
import SvgIcon from '@/components/SvgIcon.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, ref } from 'vue'

type SortDirection = 'none' | 'ascending' | 'descending'

const props = defineProps<{ entries: Entry[] }>()
const emit = defineEmits<{
  clickEntry: [entry: Entry, keepOpen: boolean, htmlElement: HTMLElement]
  scroll: []
}>()

const sortDirection = ref<SortDirection>('none')
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
    case 'ascending': {
      return [...props.entries].sort((a, b) => lexicalCmp(a.value, b.value))
    }
    case 'descending': {
      return [...props.entries].sort((a, b) => lexicalCmp(b.value, a.value))
    }
    case 'none':
    default: {
      return props.entries
    }
  }
})

const styleVars = computed(() => {
  return {
    // Slightly shift the top border of drawn dropdown away from node's top border by a fraction of
    // a pixel, to prevent it from poking through and disturbing node's siluette.
    '--extend-margin': `${0.2 / (graphNavigator?.scale ?? 1)}px`,
  }
})

function handleClick(entry: Entry, altKey: boolean, htmlElement: EventTarget | null) {
  if (htmlElement instanceof HTMLElement) emit('clickEntry', entry, altKey, htmlElement)
}
</script>

<script lang="ts">
export interface DropdownEntry {
  readonly value: string
  readonly key?: string | undefined
  readonly selected: boolean
  readonly icon?: Icon | undefined
}
</script>

<template>
  <div class="DropdownWidget" :style="styleVars">
    <ul class="list scrollable" @wheel.stop.passive @scroll="emit('scroll')">
      <li v-for="entry in sortedValues" :key="entry.key ?? entry.value">
        <button
          :class="{ selected: entry.selected }"
          class="item clickable"
          @pointerdown.prevent
          @click.stop="handleClick(entry, $event.altKey, $event.currentTarget)"
          @keydown.enter.stop
        >
          <SvgIcon v-if="entry.icon" :name="entry.icon" class="menu-icon" />
          <span class="itemContent" v-text="entry.value"></span>
        </button>
      </li>
    </ul>
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
  background-color: var(--dropdown-bg);
  border-radius: calc(var(--item-height) / 2 + var(--dropdown-padding));
  color: var(--dropdown-fg);
  /* Clip content, including selection highlight and scrollbar */
  overflow: clip;
}

/** 
 * Optional class that extends the dropdown upwards, so that it nicely merges with the node’s port.
 * Normally, only dropdowns that directly attached to a port are extended. 
 */
.ExtendUpwards {
  margin-top: calc(0px - var(--dropdown-extend));
  padding-top: var(--dropdown-extend);

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
  width: 100%;
  max-width: 100%;
  overflow: hidden;
  display: flex;
  align-items: center;

  &:hover {
    background-color: var(--dropdown-item-hover-bg);
    .itemContent {
      --text-scroll-max: calc(var(--dropdown-max-width) - 28px);
      will-change: transform;
      animation: 6s 1s infinite text-scroll;
    }
  }

  &.selected {
    background-color: var(--dropdown-item-selected-bg);

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
  overflow: clip;
  vertical-align: middle;
  margin: 3px 0;
  text-wrap: nowrap;
  text-overflow: ellipsis;
}

.menu-icon {
  margin-left: -4px;
  margin-right: 6px;
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
