<script setup lang="ts" generic="T">
import { useScrolling } from '@/components/ComponentBrowser/scrolling'
import { useApproach } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { cloneVNode, computed, h, ref, VNode, watch } from 'vue'

// const ITEM_SIZE = 32

const props = defineProps<{
  items: readonly T[]
  itemHeight: number
  autoSelectFirst: boolean
}>()
const emit = defineEmits<{
  itemAccepted: [item: T, index: number]
  'update:selectedItem': [selected: T | null, index: number | null]
}>()
const slots = defineSlots<{
  default(props: { item: T }): any
  selected(props: { item: T }): any
}>()

const scroller = ref<HTMLElement>()

// === Components List and Positions ===

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPos = scrolling.scrollPosition.value
  const topmostVisible = itemAtY(scrollPos)
  const bottommostVisible = Math.max(0, itemAtY(scrollPos + scrollerSize.value.y))
  return props.items.slice(topmostVisible, bottommostVisible + 1).map((item, i) => {
    return { item, index: i + topmostVisible }
  })
})

function createVNodes(slot: typeof slots.default, extraProperties: Record<string, unknown> = {}) {
  if (!slot) return undefined
  return visibleComponents.value.map(({ item, index }) => {
    return {
      node: h(
        'div',
        { class: 'item', style: itemStyle(index) },
        slot({ item }).map((node: VNode<unknown, unknown>) => cloneVNode(node, extraProperties)),
      ),
      item,
      index,
    }
  })
}

const nodes = computed(() => createVNodes(slots.default))
const selectionNodes = computed(() =>
  createVNodes(slots.selected ?? slots.default, { class: 'selected' }),
)

function ItemPos(index: number) {
  return index * props.itemHeight
}

function itemAtY(pos: number) {
  return Math.floor(pos / props.itemHeight)
}

function itemStyle(index: number) {
  // TODO[ao]: for some reason, position: absolute must be set here,
  //  because it sometimes is not set (.item class is not sufficient). To investigate.
  return { position: 'absolute', transform: `translateY(${ItemPos(index)}px)` }
}

// === Highlight ===

const selected = ref<number | null>(null)
const highlightPosition = ref(0)
const selectedPosition = computed(() => (selected.value != null ? ItemPos(selected.value) : null))
const highlightHeight = computed(() => (selected.value != null ? props.itemHeight : 0))
const animatedHighlightPosition = useApproach(highlightPosition)
const animatedHighlightHeight = useApproach(highlightHeight)

const selectedItem = computed(() => {
  if (selected.value === null) return null
  return props.items[selected.value] ?? null
})

watch(selectedItem, (item) => emit('update:selectedItem', item, selected.value))

watch(selectedPosition, (newPos) => {
  if (newPos == null) return
  highlightPosition.value = newPos
})

const highlightClipPath = computed(() => {
  const height = animatedHighlightHeight.value
  const position = animatedHighlightPosition.value
  const top = position + props.itemHeight - height
  const bottom = listContentHeight.value - position - props.itemHeight
  return `inset(${top}px 0px ${bottom}px 0px round 18px)`
})

function selectWithoutScrolling(index: number) {
  const scrollPos = scrolling.scrollPosition.value
  scrolling.targetScroll.value = { type: 'offset', offset: scrollPos }
  selected.value = index
}

// === Scrolling ===

const scrollerSize = useResizeObserver(scroller)
const listContentHeight = computed(() =>
  Math.max(props.items.length * props.itemHeight, scrollerSize.value.y),
)
const scrolling = useScrolling(() =>
  Math.min(animatedHighlightPosition.value, listContentHeight.value - scrollerSize.value.y),
)

const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function updateScroll() {
  // If the scrollTop value changed significantly, that means the user is scrolling.
  if (scroller.value && Math.abs(scroller.value.scrollTop - scrolling.scrollPosition.value) > 1.0) {
    scrolling.targetScroll.value = { type: 'offset', offset: scroller.value.scrollTop }
  }
}

// === Filtering Changes ===

watch(
  () => props.items,
  () => {
    selected.value = props.autoSelectFirst ? 0 : null
    scrolling.targetScroll.value = { type: 'top' }

    // Update `highlightPosition` synchronously, so the subsequent animation `skip` have an effect.
    if (selectedPosition.value != null) {
      highlightPosition.value = selectedPosition.value
    }
    animatedHighlightPosition.skip()
    animatedHighlightHeight.skip()
  },
)

// === Expose ===

defineExpose({
  moveUp() {
    if (selected.value != null && selected.value > 0) {
      selected.value -= 1
    }
    scrolling.scrollWithTransition({ type: 'selected' })
  },
  moveDown() {
    if (selected.value == null) {
      selected.value = 0
    } else if (selected.value < props.items.length - 1) {
      selected.value += 1
    }
    scrolling.scrollWithTransition({ type: 'selected' })
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
      :scrollTop.prop="scrolling.scrollPosition.value"
      @wheel.stop.passive
      @scroll="updateScroll"
    >
      <div class="list-variant">
        <component
          :is="node"
          v-for="{ node, item, index } in nodes"
          :key="index"
          @mousemove="selectWithoutScrolling(index)"
          @click="emit('itemAccepted', item, index)"
        />
      </div>
      <div class="list-variant selected" :style="{ clipPath: highlightClipPath }">
        <component
          :is="node"
          v-for="{ node, item, index } in selectionNodes"
          :key="index"
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

.list-variant {
  top: 0px;
  width: 100%;
  height: var(--list-height);
  position: absolute;
}
</style>
