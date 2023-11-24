<script lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { setDragImageToBlank } from '@/util/drag'
import { defineKeybinds } from '@/util/shortcuts'
import { computed, onMounted, onUnmounted, ref, type Ref } from 'vue'

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

const shouldRemove = ref(false)
const dragIndex = ref<number>()
const dragDisplayIndex = ref<number>()

// The type assertions are required to prevent Vue from wrapping `T` with `UnwrapSimpleRef`.
const displayedChildren = computed(() => {
  if (dragIndex.value == null) return props.modelValue
  if (shouldRemove.value) {
    const result = [...props.modelValue]
    result.splice(dragIndex.value, 1)
    return result
  }
  if (dragDisplayIndex.value == null || dragIndex.value === dragDisplayIndex.value)
    return props.modelValue
  const result = [...props.modelValue]
  const item = props.modelValue[dragIndex.value]!
  result.splice(dragIndex.value, 1)
  result.splice(dragDisplayIndex.value, 0, item)
  return result
})
const dragPos = ref({ x: 0, y: 0 })
const itemNodes = ref<HTMLLIElement[]>([])
const childBoundingBoxes = ref([]) as Ref<DOMRect[]>

const rootNode = ref<HTMLElement>()

const X_LENIENCE_PX = 2
const Y_LENIENCE_PX = 8
const REMOVE_DISTANCE_THRESHOLD_SQUARED_PX = 32 * 32

function updateBoundingBoxes() {
  childBoundingBoxes.value = itemNodes.value
    .map((element) => element.getBoundingClientRect())
    .sort(({ left: a }, { left: b }) => a - b)
}

const mouseHandler = bindings.handler({
  dragListItem(event) {
    if (!(event instanceof DragEvent)) return
    setDragImageToBlank(event)
    if (event.dataTransfer) {
      event.dataTransfer.effectAllowed = 'move'
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
  let minDistanceSquared = Infinity
  const insertIndex = childBoundingBoxes.value.findIndex((bbox) => {
    const dx = Math.max(bbox.left - event.clientX, event.clientX - bbox.right)
    const dy = Math.max(bbox.top - event.clientY, event.clientY - bbox.bottom)
    minDistanceSquared = Math.min(
      minDistanceSquared,
      (dx < 0 ? 0 : dx * dx) + (dy < 0 ? 0 : dy * dy),
    )
    return bbox && dx <= X_LENIENCE_PX && dy <= Y_LENIENCE_PX
  })
  const newShouldRemove = minDistanceSquared > REMOVE_DISTANCE_THRESHOLD_SQUARED_PX
  event.dataTransfer.dropEffect = newShouldRemove ? 'none' : 'move'
  if (
    (insertIndex === -1 || dragDisplayIndex.value === insertIndex) &&
    newShouldRemove === shouldRemove.value
  )
    return
  dragDisplayIndex.value = insertIndex
  shouldRemove.value = newShouldRemove
  requestAnimationFrame(updateBoundingBoxes)
}

function cleanupDrag() {
  shouldRemove.value = false
  dragIndex.value = undefined
  dragDisplayIndex.value = undefined
}

function onDragStart(event: DragEvent, index: number) {
  dragIndex.value = index
  if (mouseHandler(event, false)) return
  dragIndex.value = undefined
}

function onDragOver(event: DragEvent) {
  dragPos.value = { x: event.clientX, y: event.clientY }
  handleDrag(event)
}

function onDrop(event: DragEvent) {
  if (dragIndex.value === dragDisplayIndex.value) return
  event.preventDefault()
  emit('update:modelValue', displayedChildren.value)
  cleanupDrag()
}

const onDragEnd = cleanupDrag

onMounted(() => {
  document.body.addEventListener('dragover', onDragOver)
  document.body.addEventListener('drop', onDrop)
  document.body.addEventListener('dragend', onDragEnd)
})

onUnmounted(() => {
  document.body.removeEventListener('dragover', onDragOver)
  document.body.removeEventListener('drop', onDrop)
  document.body.removeEventListener('dragend', onDragEnd)
})
</script>

<template>
  <div
    ref="rootNode"
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
            :class="{ dragging: !shouldRemove && index === dragDisplayIndex }"
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
    <template v-if="dragIndex != null">
      <slot
        v-if="$slots.dragPreview"
        name="dragPreview"
        :item="modelValue[dragIndex]!"
        :x="dragPos.x"
        :y="dragPos.y"
      ></slot>
      <div
        v-else
        class="drag-preview"
        :style="{ transform: `translate(${dragPos.x}px, ${dragPos.y}px)` }"
      >
        <slot :item="modelValue[dragIndex]!"></slot>
      </div>
    </template>
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

.item.dragging {
  opacity: 0;
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

.drag-preview {
  position: fixed;
}
</style>
