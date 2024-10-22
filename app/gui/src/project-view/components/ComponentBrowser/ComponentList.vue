<script setup lang="ts">
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { useScrolling } from '@/components/ComponentBrowser/scrolling'
import SvgIcon from '@/components/SvgIcon.vue'
import { useApproach } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { groupColorStyle } from '@/composables/nodeColors'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/data/array'
import { allRanges } from '@/util/data/range'
import { computed, ref, watch } from 'vue'

const ITEM_SIZE = 32

const props = defineProps<{
  filtering: Filtering
  autoSelectFirstComponent: boolean
}>()
const emit = defineEmits<{
  acceptSuggestion: [suggestion: Component]
  'update:selectedComponent': [selected: Component | null]
}>()

const suggestionDbStore = useSuggestionDbStore()

// === Components List and Positions ===

const components = computed(() => makeComponentList(suggestionDbStore.entries, props.filtering))

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPos = scrolling.scrollPosition.value
  const topmostVisible = componentAtY(scrollPos)
  const bottommostVisible = Math.max(0, componentAtY(scrollPos + scrollerSize.value.y))
  return components.value.slice(topmostVisible, bottommostVisible + 1).map((component, i) => {
    return { component, index: i + topmostVisible }
  })
})

function componentPos(index: number) {
  return index * ITEM_SIZE
}

function componentAtY(pos: number) {
  return Math.floor(pos / ITEM_SIZE)
}

function componentStyle(index: number) {
  return { transform: `translateY(${componentPos(index)}px)` }
}

/** Group colors are populated in `GraphEditor`, and for each group in suggestion database a CSS variable is created. */
function componentColor(component: Component): string {
  return groupColorStyle(tryGetIndex(suggestionDbStore.groups, component.group))
}

// === Highlight ===

const selected = ref<number | null>(null)
const highlightPosition = ref(0)
const selectedPosition = computed(() =>
  selected.value != null ? componentPos(selected.value) : null,
)
const highlightHeight = computed(() => (selected.value != null ? ITEM_SIZE : 0))
const animatedHighlightPosition = useApproach(highlightPosition)
const animatedHighlightHeight = useApproach(highlightHeight)

const selectedComponent = computed(() => {
  if (selected.value === null) return null
  return components.value[selected.value] ?? null
})

watch(selectedComponent, (component) => emit('update:selectedComponent', component))

watch(selectedPosition, (newPos) => {
  if (newPos == null) return
  highlightPosition.value = newPos
})

const highlightClipPath = computed(() => {
  const height = animatedHighlightHeight.value
  const position = animatedHighlightPosition.value
  const top = position + ITEM_SIZE - height
  const bottom = listContentHeight.value - position - ITEM_SIZE
  return `inset(${top}px 0px ${bottom}px 0px round 16px)`
})

function selectWithoutScrolling(index: number) {
  const scrollPos = scrolling.scrollPosition.value
  scrolling.targetScroll.value = { type: 'offset', offset: scrollPos }
  selected.value = index
}

// === Scrolling ===

const scroller = ref<HTMLElement>()
const scrollerSize = useResizeObserver(scroller)
const listContentHeight = computed(() =>
  Math.max(components.value.length * ITEM_SIZE, scrollerSize.value.y),
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
  () => props.filtering,
  () => {
    selected.value = props.autoSelectFirstComponent ? 0 : null
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
    } else if (selected.value < components.value.length - 1) {
      selected.value += 1
    }
    scrolling.scrollWithTransition({ type: 'selected' })
  },
})
</script>

<template>
  <div class="ComponentList" :style="{ '--list-height': listContentHeightPx }">
    <div
      ref="scroller"
      class="list"
      :scrollTop.prop="scrolling.scrollPosition.value"
      @wheel.stop.passive
      @scroll="updateScroll"
    >
      <div class="list-variant">
        <div
          v-for="item in visibleComponents"
          :key="item.component.suggestionId"
          class="component"
          :style="componentStyle(item.index)"
          @mousemove="selectWithoutScrolling(item.index)"
          @click="emit('acceptSuggestion', item.component)"
        >
          <SvgIcon :name="item.component.icon" :style="{ color: componentColor(item.component) }" />
          <span>
            <span v-if="!item.component.matchedRanges" v-text="item.component.label"></span>
            <span
              v-for="range in allRanges(item.component.matchedRanges, item.component.label.length)"
              v-else
              :key="`${range.start},${range.end}`"
              class="component-label-segment"
              :class="{ match: range.isMatch }"
              v-text="item.component.label.slice(range.start, range.end)"
            ></span>
          </span>
        </div>
      </div>
      <div class="list-variant selected" :style="{ clipPath: highlightClipPath }">
        <div
          v-for="item in visibleComponents"
          :key="item.component.suggestionId"
          class="component"
          :style="{
            backgroundColor: componentColor(item.component),
            ...componentStyle(item.index),
          }"
          @click="emit('acceptSuggestion', item.component)"
        >
          <SvgIcon :name="item.component.icon" />
          <span>
            <span v-if="!item.component.matchedRanges" v-text="item.component.label"></span>
            <span
              v-for="range in allRanges(item.component.matchedRanges, item.component.label.length)"
              v-else
              :key="`${range.start},${range.end}`"
              class="component-label-segment"
              :class="{ match: range.isMatch }"
              v-text="item.component.label.slice(range.start, range.end)"
            ></span>
          </span>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.ComponentList {
  --list-height: 0px;
  width: 100%;
  height: 380px;
  /* position: absolute; */
  border: none;
  border-radius: var(--radius-default);
  background-color: var(--background-color);
}

.list {
  width: 100%;
  height: 100%;
  overflow-x: hidden;
  overflow-y: auto;
  position: relative;
}

.list-variant {
  top: 0px;
  width: 100%;
  height: var(--list-height);
  position: absolute;
}

.component {
  width: 100%;
  height: 32px;
  flex-direction: row;
  align-items: center;
  gap: 8px;
  padding: 9px;
  display: flex;
  position: absolute;
  line-height: 1;
  font-family: var(--font-code);
}

.selected {
  color: white;
  & svg {
    color: white;
  }
}

.component-label-segment.match {
  font-weight: bold;
}
</style>
