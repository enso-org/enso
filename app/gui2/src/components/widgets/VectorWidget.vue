<script lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { defineKeybinds } from '@/util/shortcuts'
import { computed, ref, watch, watchEffect, type Ref } from 'vue'

const bindings = defineKeybinds('vector-widget', {
  dragListItem: ['PointerMain', 'Mod+PointerMain'],
})
</script>

<script setup lang="ts" generic="T">
const props = defineProps<{
  modelValue: T[]
  default: () => T
  getKey?: (item: T) => PropertyKey | undefined
  /** If present, a {@link DataTransferItem} is added with a MIME type of `text/plain`.
   * This is useful if the drag payload has a representation that can be pasted in terminals,
   * search bars, and/or address bars. */
  toPlainText?: (item: T) => string
  /** The MIME type for the payload output added by `toDragPayload`.
   * Unused if `toDragPayload` is not also present.
   * When in doubt, this should be `application/json`.
   * Defaults to `application/octet-stream`, meaning the payload is arbitrary binary data. */
  dragMimeType?: string
  /** When in doubt, this should be `JSON.stringify` of data describing the object. */
  toDragPayload?: (item: T) => string
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: T[]] }>()

const mimeType = computed(() => props.dragMimeType ?? 'application/octet-stream')

const DRAG_PREVIEW_CLASS = 'drag-preview'

const dragItem = ref<T>()
const dragIndex = ref<number>()

function withIndex<T>(array: T[]) {
  return array.map((item, index) => ({ item, index }))
}

// The type assertions are required to prevent Vue from wrapping `T` with `UnwrapSimpleRef`.
const displayedChildren = ref(withIndex(props.modelValue)) as Ref<{ item: T; index: number }[]>
const itemNodes = ref(
  props.modelValue.map((_, index) => ({
    index,
    element: document.createElement('li'),
  })),
)
const childBoundingBoxes = ref([]) as Ref<{ index: number; boundingBox: DOMRect | undefined }[]>

watchEffect(() => (displayedChildren.value = withIndex(props.modelValue)))
watch(
  displayedChildren,
  () =>
    (itemNodes.value = props.modelValue.map((_, index) => ({
      index,
      element: document.createElement('li'),
    }))),
)

function updateBoundingBoxes() {
  childBoundingBoxes.value = itemNodes.value.map(({ index, element }) => ({
    index,
    boundingBox: element?.getBoundingClientRect(),
  }))
}

const mouseHandler = bindings.handler({
  dragListItem(event) {
    if (!(event instanceof DragEvent)) return
    const target = event.target instanceof HTMLElement && event.target
    if (target) {
      target.classList.add(DRAG_PREVIEW_CLASS)
      requestAnimationFrame(() => target.classList.remove(DRAG_PREVIEW_CLASS))
    }
    if (event.dataTransfer) {
      event.dataTransfer.effectAllowed = 'move'
      if (dragItem.value && props.toPlainText) {
        event.dataTransfer.setData('text/plain', props.toPlainText(dragItem.value))
      }
      if (dragItem.value && props.toDragPayload) {
        event.dataTransfer.setData(mimeType.value, props.toDragPayload(dragItem.value))
      }
    }
    updateBoundingBoxes()
  },
})

function handleDragAndReturnChildList(event: DragEvent) {
  const item = dragItem.value
  const itemIndex = dragIndex.value
  if (
    !event.dataTransfer ||
    !item ||
    itemIndex == null ||
    (props.toDragPayload && !event.dataTransfer?.types.includes(mimeType.value))
  )
    return displayedChildren.value
  event.dataTransfer.dropEffect = 'move'
  event.preventDefault()
  const nextSibling = childBoundingBoxes.value.find(
    ({ boundingBox }) =>
      boundingBox && event.clientX >= boundingBox.left && event.clientX <= boundingBox.right,
  )
  const insertIndex = nextSibling?.index ?? 0
  const displayIndex = displayedChildren.value.findIndex(({ index }) => index === itemIndex)
  console.log('d', displayIndex, 'i', insertIndex, 'I', itemIndex)
  if (displayIndex === insertIndex) return displayedChildren.value
  const newChildren = withIndex(props.modelValue)
  newChildren.splice(itemIndex, 1)
  newChildren.splice(insertIndex + (insertIndex < itemIndex ? -1 : 0), 0, {
    item,
    index: itemIndex,
  })
  console.log(childBoundingBoxes.value.map((i) => i.index))
  setTimeout(updateBoundingBoxes, 100)
  return newChildren
}

function onDragStart(event: DragEvent, item: T, originalIndex: number) {
  dragItem.value = item
  dragIndex.value = originalIndex
  if (mouseHandler(event, false)) return
  dragItem.value = undefined
  dragIndex.value = undefined
}

function onDragOver(event: DragEvent) {
  displayedChildren.value = handleDragAndReturnChildList(event)
}

function onDrop(event: DragEvent) {
  emit(
    'update:modelValue',
    handleDragAndReturnChildList(event).map(({ item }) => item),
  )
  dragItem.value = undefined
  dragIndex.value = undefined
}
</script>

<template>
  <div
    class="VectorWidget"
    @pointerdown="
      !$event.shiftKey && !$event.altKey && !$event.metaKey && $event.stopImmediatePropagation()
    "
  >
    <div class="vector-literal literal" @dragover="onDragOver" @drop="onDrop">
      <span class="token">[</span>
      <TransitionGroup tag="ul" name="list" class="items">
        <template
          v-for="({ item, index: originalIndex }, index) in displayedChildren"
          :key="props.getId?.(item) ?? originalIndex"
        >
          <li v-if="index !== 0" class="token">,&nbsp;</li>
          <li
            :ref="
              (el) => (itemNodes[index] = { index: originalIndex, element: el as HTMLLIElement })
            "
            class="item"
            :class="{ dragging: item === dragItem }"
            draggable="true"
            @dragstart="onDragStart($event, item, originalIndex)"
          >
            <slot :item="item"></slot>
          </li>
        </template>
      </TransitionGroup>
      <span class="token">]</span>
    </div>
    <SvgIcon
      class="add-item"
      name="vector_add"
      @pointerdown="
        !$event.ctrlKey &&
          !$event.shiftKey &&
          !$event.altKey &&
          !$event.metaKey &&
          $event.stopImmediatePropagation()
      "
      @click="emit('update:modelValue', [...props.modelValue, props.default()])"
    />
  </div>
</template>

<style scoped>
.VectorWidget {
  display: flex;
  gap: 8px;
  align-items: center;
}

/* FIXME [sb]: Vue transforms are broken when the CSS scale is not 1:
 * https://github.com/vuejs/core/issues/9665 */
/* .list-move,
.list-enter-active,
.list-leave-active {
  transition: transform 0.5s ease;
} */

.list-enter-from,
.list-leave-to {
  width: 0;
}

.vector-literal {
  display: flex;
}

ul {
  display: flex;
}

.token {
  color: rgb(255 255 255 / 0.33);
  vertical-align: middle;
  align-items: center;
  display: inline-flex;
  user-select: none;
  height: 24px;
}

/* FIXME: `opacity: 0`, a copy of the dragged element should follow the mouse */
.item.dragging {
  opacity: 0.5;
}

.add-item {
  transition-property: opacity;
  transition-duration: 150ms;
  transition-timing-function: ease-in-out;
  opacity: 0.8;
}

.add-item:hover {
  opacity: 1;
}
</style>
