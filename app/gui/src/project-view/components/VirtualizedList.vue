<script setup lang="ts" generic="T">
/**
 *  @file A list component which instantiates visible elements only.
 *
 * The element provided by default slot is used for rendering elements. Only visible elements
 * are instantiated. See usage in ComponentList as an example.
 */
import { listBindings } from '@/bindings'
import { useApproach } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import * as objects from 'enso-common/src/utilities/data/object'
import { cloneVNode, computed, h, ref, useCssModule, watch, type VNode } from 'vue'

const selected = defineModel<number | null>('selected', { required: false, default: null })
const {
  items,
  itemHeight,
  scrollToSelectionMargin = 0.0,
  autoSelectFirst = false,
  debounceMouseSelection,
} = defineProps<{
  /** List of models for an item. */
  items: readonly T[]
  itemHeight: number
  /** A margin kept when scrolling while navigating with arrows */
  scrollToSelectionMargin?: number
  /** When set to true, on every `list` prop update first element will be selecetd (if any) */
  autoSelectFirst?: boolean
  /**
   * Debounce time for selecting with mouse hover, expressed in milliseconds.
   *
   * Navigating with arrows will still update `selected` model immediately.
   */
  debounceMouseSelection?: number
}>()
const emit = defineEmits<{
  itemAccepted: [item: T, index: number]
}>()
const slots = defineSlots<{
  default(props: { item: T; selected: boolean }): any
}>()
const style = useCssModule()

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
    const selected = index === highlighted.value
    return {
      node: h(
        'div',
        { class: style.item, style: itemStyle(index) },
        slot({ item, selected }).map((node: VNode<unknown, unknown>) =>
          cloneVNode(node, { class: { selected } }),
        ),
      ),
      item,
      index,
    }
  })
}

const nodes = computed(() => createVNodes(slots.default))

function itemPos(index: number) {
  return index * itemHeight
}

function itemAtY(pos: number) {
  return Math.floor(pos / itemHeight)
}

function itemStyle(index: number) {
  return { transform: `translateY(${itemPos(index)}px)` }
}

// === Selection ===

const highlighted = ref<number | null>(selected.value)
let mouseSelectionDebounce: ReturnType<typeof setTimeout> | undefined

function updateSelectionToHighlight() {
  clearTimeout(mouseSelectionDebounce)
  mouseSelectionDebounce = undefined
  selected.value = highlighted.value
}

function cancelMouseHoverSelection() {
  clearTimeout(mouseSelectionDebounce)
  highlighted.value = selected.value
}

watch(selected, (x) => (highlighted.value = x))
watch(highlighted, () => {
  clearTimeout(mouseSelectionDebounce)
  if (debounceMouseSelection == null) {
    updateSelectionToHighlight()
  } else {
    mouseSelectionDebounce = setTimeout(updateSelectionToHighlight, debounceMouseSelection)
  }
})

// === Scrolling ===

const scrollerSize = useResizeObserver(scroller)
const listContentHeight = computed(() => Math.max(items.length * itemHeight, scrollerSize.value.y))
const scrollTarget = ref(0.0)
const scrollPosition = useApproach(scrollTarget)
const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function showHighlightedItem() {
  if (highlighted.value == null) return
  const highlightedPosition = itemPos(highlighted.value)
  const maxScrollPos = Math.max(highlightedPosition - scrollToSelectionMargin, 0.0)
  const minScrollPos = Math.min(
    highlightedPosition + itemHeight + scrollToSelectionMargin - scrollerSize.value.y,
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
    selected.value = autoSelectFirst && items.length > 0 ? 0 : null
    scrollTarget.value = 0.0
  },
)

// === Expose ===

function moveUp() {
  if (highlighted.value != null && highlighted.value > 0) {
    highlighted.value -= 1
  }
  updateSelectionToHighlight()
  showHighlightedItem()
}

function moveDown() {
  if (highlighted.value == null) {
    highlighted.value = 0
  } else if (highlighted.value < items.length - 1) {
    highlighted.value += 1
  }
  updateSelectionToHighlight()
  showHighlightedItem()
}

function accept() {
  updateSelectionToHighlight()
  if (selected.value == null) return false
  const item = items[selected.value]
  if (item == null) return false
  emit('itemAccepted', item, selected.value)
}

const actions = registerHandlers({
  'list.moveUp': { action: moveUp },
  'list.moveDown': { action: moveDown },
  'list.accept': { action: accept },
})

const handler = listBindings.handler(
  objects.mapEntries(listBindings.bindings, (actionName) => actions[actionName].action),
)
defineExpose({ moveUp, moveDown, accept })
</script>

<template>
  <div
    class="LazyList"
    :style="{ '--list-height': listContentHeightPx, '--item-height': itemHeight }"
    @keydown="handler"
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
          @mousemove="highlighted = index"
          @mouseleave="cancelMouseHoverSelection"
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
<style module>
.item {
  position: absolute;
  width: 100%;
  height: var(--item-height);
  min-height: var(--item-height);
  max-height: var(--item-height);
}
</style>
