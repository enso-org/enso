<script setup lang="ts" generic="T">
import { useApproach } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { cloneVNode, computed, h, ref, VNode, watch } from 'vue'

const {
  items,
  itemHeight,
  scrollToSelectionMargin = 0.0,
  autoSelectFirst = false,
} = defineProps<{
  items: readonly T[]
  itemHeight: number
  scrollToSelectionMargin?: number
  autoSelectFirst?: boolean
}>()
const emit = defineEmits<{
  itemAccepted: [item: T, index: number]
  'update:selectedItem': [selected: T | null, index: number | null]
}>()
const slots = defineSlots<{
  default(props: { item: T }): any
}>()

const scroller = ref<HTMLElement>()

// === Components List and Positions ===

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPos = scrollPosition.value
  const topmostVisible = itemAtY(scrollPos)
  const bottommostVisible = Math.max(0, itemAtY(scrollPos + scrollerSize.value.y))
  return items.slice(topmostVisible, bottommostVisible + 1).map((item, i) => {
    return { item, index: i + topmostVisible }
  })
})

function createVNodes(slot: typeof slots.default) {
  if (!slot) return undefined
  return visibleComponents.value.map(({ item, index }) => {
    return {
      node: h(
        'div',
        { class: 'item', style: itemStyle(index) },
        slot({ item }).map((node: VNode<unknown, unknown>) =>
          cloneVNode(node, { class: { selected: index === selected.value } }),
        ),
      ),
      item,
      index,
    }
  })
}

const nodes = computed(() => createVNodes(slots.default))

function ItemPos(index: number) {
  return index * itemHeight
}

function itemAtY(pos: number) {
  return Math.floor(pos / itemHeight)
}

function itemStyle(index: number) {
  // TODO[ao]: for some reason, .item class style must be repeated here,
  //  because the class has no effect otherwise. To investigate.
  return {
    position: 'absolute',
    width: '100%',
    height: 'var(--item-height)',
    minHeight: 'var(--item-height)',
    maxHeight: 'var(--item-height)',
    transform: `translateY(${ItemPos(index)}px)`,
  }
}

// === Highlight ===

const selected = ref<number | null>(null)
const selectedPosition = computed(() => (selected.value != null ? ItemPos(selected.value) : null))

const selectedItem = computed(() => {
  if (selected.value === null) return null
  return items[selected.value] ?? null
})

watch(selectedItem, (item) => emit('update:selectedItem', item, selected.value))

// === Scrolling ===

const scrollerSize = useResizeObserver(scroller)
const listContentHeight = computed(() => Math.max(items.length * itemHeight, scrollerSize.value.y))
const scrollTarget = ref(0.0)
const scrollPosition = useApproach(scrollTarget)
const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function showSelectedItem() {
  if (selectedPosition.value == null) return
  const maxScrollPos = Math.max(selectedPosition.value - scrollToSelectionMargin, 0.0)
  const minScrollPos = Math.min(
    selectedPosition.value + itemHeight + scrollToSelectionMargin - scrollerSize.value.y,
    listContentHeight.value - scrollerSize.value.y,
  )
  if (scrollPosition.value > maxScrollPos) {
    scrollTarget.value = maxScrollPos
  } else if (scrollPosition.value < minScrollPos) {
    scrollTarget.value = minScrollPos
  }
}

function updateScroll() {
  // If the scrollTop value changed significantly, that means the user is scrolling.
  if (scroller.value && Math.abs(scroller.value.scrollTop - scrollPosition.value) > 1.0) {
    scrollTarget.value = scroller.value.scrollTop
    scrollPosition.skip()
  }
}

// === Filtering Changes ===

watch(
  () => items,
  () => {
    selected.value = autoSelectFirst ? 0 : null
    scrollTarget.value = 0.0
  },
)

// === Expose ===

defineExpose({
  moveUp() {
    if (selected.value != null && selected.value > 0) {
      selected.value -= 1
    }
    showSelectedItem()
  },
  moveDown() {
    if (selected.value == null) {
      selected.value = 0
    } else if (selected.value < items.length - 1) {
      selected.value += 1
    }
    showSelectedItem()
  },
})
</script>

<template>
  <div
    class="LazyList"
    :style="{ '--list-height': listContentHeightPx, '--item-height': itemHeight }"
  >
    <div
      ref="scroller"
      class="list"
      :scrollTop.prop="scrollPosition.value"
      @wheel.stop.passive
      @scroll="updateScroll"
    >
      <div class="list-content">
        <component
          :is="node"
          v-for="{ node, item, index } in nodes"
          :key="index"
          class="item"
          @mousemove="selected = index"
          @click="emit('itemAccepted', item, index)"
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.LazyList {
  --list-height: 0px;
  --item-height: 32px;
}

.item {
  position: absolute;
  width: 100%;
  height: var(--item-height);
  min-height: var(--item-height);
  max-height: var(--item-height);
}

.list {
  width: 100%;
  height: 100%;
  overflow-x: hidden;
  overflow-y: scroll;
  position: relative;
}

.list-content {
  top: 0px;
  width: 100%;
  height: var(--list-height);
  position: absolute;
}
</style>
