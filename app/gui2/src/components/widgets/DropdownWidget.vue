<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { Icon } from '@/util/iconName'
import { computed, onMounted, onUnmounted, ref } from 'vue'

enum SortDirection {
  none = 'none',
  ascending = 'ascending',
  descending = 'descending',
}

const props = defineProps<{ color: string; selectedValue: string | undefined; values: string[] }>()
const emit = defineEmits<{ click: [index: number, keepOpen: boolean] }>()

const tree = injectWidgetTree()

let endClippingInhibition: (() => void) | undefined
onMounted(() => {
  endClippingInhibition = tree.inhibitClipping()
})
onUnmounted(() => endClippingInhibition?.())

const sortDirection = ref<SortDirection>(SortDirection.none)

const sortedValuesAndIndices = computed(() => {
  const valuesAndIndices = props.values.map<[value: string, index: number]>((value, index) => [
    value,
    index,
  ])
  switch (sortDirection.value) {
    case SortDirection.ascending: {
      return valuesAndIndices.sort((a, b) =>
        a[0] > b[0] ? 1
        : a[0] < b[0] ? -1
        : 0,
      )
    }
    case SortDirection.descending: {
      return valuesAndIndices.sort((a, b) =>
        a[0] > b[0] ? -1
        : a[0] < b[0] ? 1
        : 0,
      )
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

// Currently unused.
const enableSortButton = ref(false)
</script>

<template>
  <div class="Dropdown" @pointerdown.stop @pointerup.stop @click.stop>
    <ul class="list scrollable" :style="{ background: color, borderColor: color }" @wheel.stop>
      <template v-for="[value, index] in sortedValuesAndIndices" :key="value">
        <li v-if="value === selectedValue">
          <div class="selected-item button" @click.stop="emit('click', index, $event.altKey)">
            <span v-text="value"></span>
          </div>
        </li>
        <li v-else class="selectable-item button" @click.stop="emit('click', index, $event.altKey)">
          <span v-text="value"></span>
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

.selectable-item:hover {
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
  margin-left: 8px;
}

.selectable-item {
  margin-right: 16px;
  padding-left: 8px;
}
</style>
