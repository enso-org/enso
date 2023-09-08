<script setup lang="ts">
import { useResizeObserver, useWindowEvent } from '@/util/events'
import { useComponentsStore } from '@/stores/components'
import type { Component } from '@/stores/components'
import type { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { computed, nextTick, ref, watch } from 'vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useApproach } from '@/util/animation'

const ITEM_SIZE = 32

const props = defineProps<{
  navigator: ReturnType<typeof useNavigator>
}>()

// === Position ===

const shown = ref(false)
const scenePosition = ref(Vec2.Zero())

const transform = computed(() => {
  const nav = props.navigator
  const pos = scenePosition.value
  return `${nav.transform} translate(${pos.x}px, ${pos.y}px) scale(${
    1 / nav.scale
  }) translateY(-100%)`
})

function positionAtMouse(): boolean {
  const nav = props.navigator
  const mousePos = nav.sceneMousePos
  if (mousePos == null) return false
  scenePosition.value = mousePos
  return true
}

// === Components List and Positions ===

const componentStore = useComponentsStore()

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPosition = animatedScrollPosition.value
  const firstVisible = componentAtY(scrollPosition)
  const lastVisible = componentAtY(animatedScrollPosition.value + scrollerSize.value.y)
  return componentStore.components.slice(firstVisible, lastVisible + 1).map((component, i) => {
    return { component, index: i + firstVisible }
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

function componentColor(component: Component): string {
  return componentStore.groups[component.group].color
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

watch(selectedPosition, (newPos) => {
  if (newPos == null) return
  highlightPosition.value = newPos
  if (animatedHighlightHeight.value <= 1.0) {
    animatedHighlightPosition.skip()
  }
})

const highlightClipPath = computed(() => {
  let height = animatedHighlightHeight.value
  let position = animatedHighlightPosition.value
  let top = position + ITEM_SIZE - height
  let bottom = listContentHeight.value - position - ITEM_SIZE
  return `inset(${top}px 0px ${bottom}px 0px round 16px)`
})

function navigateFirst() {
  selected.value = 0
  scrollToSelected()
}

function navigateLast() {
  selected.value = componentStore.components.length - 1
  scrollToSelected()
}

function navigateUp() {
  if (selected.value == null) {
    selected.value = componentStore.components.length - 1
  } else if (selected.value > 0) {
    selected.value -= 1
  }
  scrollToSelected()
}

function navigateDown() {
  if (selected.value != null && selected.value < componentStore.components.length - 1) {
    selected.value += 1
  }
  scrollToSelected()
}

// === Scrolling ===

const scroller = ref<HTMLElement>()
const scrollerSize = useResizeObserver(scroller)
const scrollPosition = ref(0)
const animatedScrollPosition = useApproach(scrollPosition)

const listContentHeight = computed(() => componentStore.components.length * ITEM_SIZE)
const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function scrollToSelected() {
  if (selectedPosition.value == null) return
  scrollPosition.value = selectedPosition.value - scrollerSize.value.y + ITEM_SIZE
}

function updateScroll() {
  if (scroller.value && Math.abs(scroller.value.scrollTop - animatedScrollPosition.value) > 1.0) {
    scrollPosition.value = scroller.value.scrollTop
    animatedScrollPosition.skip()
  }
}

// === Documentation Panel ===

const docsVisible = ref(true)

// === Key Events Handler ===

useWindowEvent('keydown', (e) => {
  switch (e.key) {
    case 'Enter':
      if (!shown.value && positionAtMouse()) {
        shown.value = true
        selected.value = componentStore.components.length - 1
        nextTick(() => {
          scrollToSelected()
          animatedScrollPosition.skip()
          animatedHighlightPosition.skip()
          // After showing, the scroll top is set to 0 despite having assigned `scrollTop.prop` in
          // the template. We need to manually assign it.
          if (scroller.value) {
            scroller.value.scrollTop = animatedScrollPosition.value
          }
        })
      } else {
        shown.value = false
      }
      break
    case 'ArrowUp':
      e.preventDefault()
      navigateUp()
      break
    case 'ArrowDown':
      e.preventDefault()
      navigateDown()
      break
    case 'Home':
      e.preventDefault()
      navigateFirst()
      break
    case 'End':
      e.preventDefault()
      navigateLast()
      break
    case 'Escape':
      console.log('ESC')
      e.preventDefault()
      selected.value = null
      break
  }
})
</script>

<template>
  <div
    v-if="shown"
    class="ComponentBrowser"
    :style="{ transform, '--list-height': listContentHeightPx }"
  >
    <div class="panel components">
      <div class="top-bar">
        <div class="top-bar-inner">
          <ToggleIcon icon="local_scope2" />
          <ToggleIcon icon="command_key3" />
          <ToggleIcon icon="unstable2" />
          <ToggleIcon icon="marketplace" />
          <ToggleIcon v-model="docsVisible" icon="right_side_panel" class="first-on-right" />
        </div>
      </div>
      <div class="components-content">
        <div
          ref="scroller"
          class="list"
          :scrollTop.prop="animatedScrollPosition.value"
          @wheel.stop
          @scroll="updateScroll"
        >
          <div class="list-variant" style="">
            <div
              v-for="item in visibleComponents"
              :key="item.component.id"
              class="component"
              :style="componentStyle(item.index)"
              @mousemove="selected = item.index"
            >
              <SvgIcon
                :name="item.component.icon"
                :style="{ color: componentColor(item.component) }"
              />
              {{ item.component.label }}
            </div>
          </div>
          <div class="list-variant selected" :style="{ clipPath: highlightClipPath }">
            <div
              v-for="item in visibleComponents"
              :key="item.component.id"
              class="component"
              :style="{
                backgroundColor: componentColor(item.component),
                ...componentStyle(item.index),
              }"
            >
              <SvgIcon :name="item.component.icon" />
              {{ item.component.label }}
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="panel docs" :class="{ hidden: !docsVisible }">DOCS</div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --list-height: 0px;
  width: fit-content;
  color: rgba(0, 0, 0, 0.6);
  display: flex;
  flex-direction: row;
  gap: 4px;
}

.panel {
  height: 380px;
  border: none;
  border-radius: 20px;
  background-color: #eaeaea;
}

.components {
  width: 190px;
  position: relative;
}

.components-content {
  position: absolute;
  top: 0px;
  padding: 4px;
  width: 100%;
  height: 100%;
}

.docs {
  width: 406px;
  clip-path: inset(0 0 0 0 round 20px);
  transition: clip-path 0.2s;
}
.docs.hidden {
  clip-path: inset(0 100% 0 0 round 20px);
}

.list {
  width: 100%;
  height: 100%;
  overflow-y: auto;
  font-size: 11.5px;
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
}
.selected {
  color: white;
  & svg {
    color: white;
  }
}

.top-bar {
  width: 100%;
  height: 40px;
  padding: 4px;
  background-color: #eaeaea;
  border-radius: 20px;
  position: absolute;
  top: 0px;
  z-index: 1;
}

.top-bar-inner {
  width: 100%;
  height: 100%;
  border-radius: 16px;
  border: 0.5px solid rgba(0, 0, 0, 0.12);
  display: flex;
  flex-direction: row;
  gap: 12px;
  padding: 7px;

  & svg {
    color: rgba(0, 0, 0, 0.18);
    transition: color 0.2s;
  }
  & .first-on-right {
    margin-left: auto;
  }
  & > svg.toggledOn {
    color: rgba(0, 0, 0, 0.6);
  }

  & > svg:not(.toggledOn):hover {
    color: rgba(0, 0, 0, 0.3);
  }
}
</style>
