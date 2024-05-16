<script setup lang="ts">
import { useEvent, useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, onMounted, ref, watch } from 'vue'

const props = defineProps<{
  selectionElement: HTMLElement
  verticalPaddingPx?: number
  horizontalOffsetPx?: number
}>()

function useSelectionBounds(boundingElement?: Element) {
  const bounds = ref<Rect>()

  function readLocation() {
    const selection = window.getSelection()
    if (
      selection !== null &&
      !selection.isCollapsed &&
      (!boundingElement || boundingElement.contains(selection.anchorNode))
    ) {
      const domRange = selection.getRangeAt(0)

      let rawBounds
      if (selection.anchorNode === boundingElement) {
        let inner = boundingElement
        while (inner.firstElementChild != null) {
          inner = inner.firstElementChild as HTMLElement
        }
        rawBounds = Rect.FromDomRect(inner.getBoundingClientRect())
      } else {
        rawBounds = Rect.FromDomRect(domRange.getBoundingClientRect())
      }

      if (boundingElement) {
        const origin = Vec2.FromXY(boundingElement.getBoundingClientRect())
        bounds.value = new Rect(rawBounds.pos.sub(origin), rawBounds.size)
      } else {
        bounds.value = rawBounds
      }
    } else {
      bounds.value = undefined
    }
  }

  useEvent(document, 'selectionchange', readLocation)

  if (boundingElement) {
    const size = useResizeObserver(ref(boundingElement))
    watch(size, readLocation)
  }

  return { bounds }
}

const selectionValid = ref(false)
const size = ref<Vec2>()
const cssTop = ref('0')
const cssLeft = ref('0')
const cssVisibility = computed(() => (size.value ? 'unset' : 'hidden'))

const rootElement = ref<HTMLElement>()

onMounted(() => {
  if (!rootElement.value) return
  size.value = Vec2.FromSize(rootElement.value.getBoundingClientRect())

  const { bounds } = useSelectionBounds(props.selectionElement)
  const editorSize = useResizeObserver(ref(props.selectionElement))
  watch([bounds, size], ([selectionBounds, size]) => {
    if (selectionValid.value != (selectionBounds != null))
      selectionValid.value = selectionBounds != null
    if (!selectionBounds || !size) return
    const verticalPaddingPx = props.verticalPaddingPx ?? 0
    const yAbove = selectionBounds.top - (size.y + verticalPaddingPx)
    if (yAbove >= 0) {
      cssTop.value = `${yAbove}px`
    } else {
      cssTop.value = `${selectionBounds.bottom + verticalPaddingPx}px`
    }
    const x = Math.min(
      selectionBounds.left + (props.horizontalOffsetPx ?? 0),
      editorSize.value.x - size.x,
    )
    cssLeft.value = `${x}px`
  })
})
</script>

<template>
  <div v-if="selectionValid || !size" ref="rootElement" class="FloatingSelectionMenu">
    <slot />
  </div>
</template>

<style scoped>
.FloatingSelectionMenu {
  position: absolute;
  top: v-bind('cssTop');
  left: v-bind('cssLeft');
  visibility: v-bind('cssVisibility');
}
</style>
