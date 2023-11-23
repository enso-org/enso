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

const dragIndex = ref<number>()
const dragDisplayIndex = ref<number>()

// The type assertions are required to prevent Vue from wrapping `T` with `UnwrapSimpleRef`.
const displayedChildren = computed(() => {
  if (
    dragIndex.value == null ||
    dragDisplayIndex.value == null ||
    dragIndex.value === dragDisplayIndex.value
  )
    return props.modelValue
  const array = [...props.modelValue]
  const item = props.modelValue[dragIndex.value]!
  array.splice(dragIndex.value, 1)
  array.splice(dragDisplayIndex.value, 0, item)
  return array
})
const itemNodes = ref<HTMLLIElement[]>([])
const childBoundingBoxes = ref([]) as Ref<DOMRect[]>

function updateBoundingBoxes() {
  childBoundingBoxes.value = itemNodes.value
    .map((element) => element.getBoundingClientRect())
    .sort(({ left: a }, { left: b }) => a - b)
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
      event.dataTransfer.dropEffect = 'move'
      if (dragIndex.value != null) {
        const dragItem = props.modelValue[dragIndex.value]!
        if (props.toPlainText) {
          event.dataTransfer.setData('text/plain', props.toPlainText(dragItem))
        }
        if (props.toDragPayload) {
          event.dataTransfer.setData(mimeType.value, props.toDragPayload(dragItem))
        }
      }
    }
    updateBoundingBoxes()
  },
})

function handleDrag(event: DragEvent) {
  const itemIndex = dragIndex.value
  if (
    !event.dataTransfer ||
    itemIndex == null ||
    (props.toDragPayload && !event.dataTransfer?.types.includes(mimeType.value))
  )
    return displayedChildren.value
  event.preventDefault()
  const insertIndex =
    childBoundingBoxes.value.findIndex(
      (bbox) => bbox && event.clientX >= bbox.left && event.clientX <= bbox.right,
    ) ?? 0
  if (dragDisplayIndex.value === insertIndex) return
  dragDisplayIndex.value = insertIndex
  requestAnimationFrame(updateBoundingBoxes)
}

function onDragStart(event: DragEvent, index: number) {
  dragIndex.value = index
  if (mouseHandler(event, false)) return
  dragIndex.value = undefined
}

function onDragOver(event: DragEvent) {
  handleDrag(event)
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (dragIndex.value !== dragDisplayIndex.value) {
    emit('update:modelValue', displayedChildren.value)
  }
}

function onDragEnd() {
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
    <div class="vector-literal literal" @dragover="onDragOver" @drop="onDrop" @dragend="onDragEnd">
      <span class="token">[</span>
      <TransitionGroup tag="ul" name="list" class="items">
        <template
          v-for="(item, index) in displayedChildren"
          :key="props.getId?.(item) ?? modelValue.indexOf(item)"
        >
          <li v-if="index !== 0" class="token">,&nbsp;</li>
          <li
            ref="itemNodes"
            class="item"
            :class="{ dragging: dragIndex && item === modelValue[dragIndex] }"
            draggable="true"
            @dragstart="onDragStart($event, index)"
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
