<script setup lang="ts">
import { useResizeObserver, useWindowEvent } from '@/util/events'
import { useComponentsStore } from '@/stores/components'
import type { Component } from '@/stores/components'
import type { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { computed, nextTick, ref } from 'vue'
import icons from '@/assets/icons.svg'
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
  if (scroller.value) {
    const first_visible = componentAtY(animatedScrollPosition.value)
    const last_visible = componentAtY(animatedScrollPosition.value + scrollerSize.value.y)
    return componentStore.components.slice(first_visible, last_visible + 1).map((component, i) => {
      return { component, index: i + first_visible }
    })
  } else {
    return []
  }
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

const selected = ref(-1)
const selectedPosition = computed(() => componentPos(selected.value))
const highlightPosition = useApproach(selectedPosition)

const highlightClipPath = computed(() => {
  let top = highlightPosition.value
  let bottom = listContentHeight.value - top - ITEM_SIZE
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
  if (selected.value > 0) {
    selected.value -= 1
  }
  scrollToSelected()
}

function navigateDown() {
  if (selected.value < componentStore.components.length - 1) {
    selected.value += 1
  }
  scrollToSelected()
}

// === Scrolling ===

const scroller = ref<HTMLElement>()
const scrollerSize = useResizeObserver(scroller)
const scrollPosition = ref(123)
const animatedScrollPosition = useApproach(scrollPosition)

const listContentHeight = computed(() => componentStore.components.length * ITEM_SIZE)
const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function scrollToSelected() {
  scrollPosition.value = selectedPosition.value - scrollerSize.value.y + ITEM_SIZE
}

function updateScroll() {
  if (scroller.value && Math.abs(scroller.value.scrollTop - animatedScrollPosition.value) > 1.0) {
    scrollPosition.value = scroller.value.scrollTop
    animatedScrollPosition.skip()
  }
}

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
          highlightPosition.skip()
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
      navigateFirst()
      break
    case 'End':
      navigateLast()
      break
  }
})
</script>

<template>
  <div class="ComponentBrowser" v-if="shown" :style="{ transform }">
    <div class="panel components">
      <div
        ref="scroller"
        class="list"
        :scrollTop.prop="animatedScrollPosition.value"
        @wheel.stop
        @scroll="updateScroll"
      >
        <div class="list-variant">
          <div
            v-for="item in visibleComponents"
            class="component"
            @mousemove="selected = item.index"
            :key="item.component.id"
            :style="componentStyle(item.index)"
          >
            <svg>
              <use
                :href="`${icons}#${item.component.icon}`"
                :style="{ color: componentColor(item.component) }"
              ></use>
            </svg>
            {{ item.component.label }}
          </div>
        </div>
        <div class="list-variant selected" :style="{ clipPath: highlightClipPath }">
          <div
            v-for="item in visibleComponents"
            class="component"
            :key="item.component.id"
            :style="{
              backgroundColor: componentColor(item.component),
              ...componentStyle(item.index),
            }"
          >
            <svg>
              <use :href="`${icons}#${item.component.icon}`"></use>
            </svg>
            {{ item.component.label }}
          </div>
        </div>
      </div>
    </div>
    <div class="panel docs">DOCS</div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  color: black;
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
  padding: 4px;
}

.docs {
  width: 406px;
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
  height: v-bind(listContentHeightPx);
  position: absolute;
}

.component {
  width: 100%;
  height: calc(v-bind(ITEM_SIZE) * 1px);
  flex-direction: row;
  align-items: center;
  gap: 8px;
  padding: 9px;
  display: flex;
  position: absolute;
}
.selected {
  color: white;
}

.selected > div > svg > use {
  color: white;
}

.component > svg {
  width: 16px;
  height: 16px;
}
</style>
