<script lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { defineKeybinds } from '@/util/shortcuts'
import { computed, ref, watch, type Ref } from 'vue'

const bindings = defineKeybinds('vector-widget', {
  dragListItem: ['PointerMain', 'Mod+PointerMain'],
})
</script>

<script setup lang="ts" generic="T">
const props = defineProps<{
  modelValue: T[]
  default: () => T
  getId?: (item: T) => PropertyKey | undefined
  dragMimeType?: string
  toString?: (item: T) => string
  toDragPayload?: (item: T) => unknown
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: T[]] }>()

const mimeType = computed(() => props.dragMimeType ?? 'application/json')

const DRAG_PREVIEW_CLASS = 'drag-preview'
const MAX_DISTANCE_PX = 32

const dragItem = ref<T>()

// The type assertion is required to prevent Vue from wrapping `T` with `UnwrapSimpleRef`.
const displayedChildren = ref([]) as Ref<T[]>
const itemNodes = ref<HTMLLIElement[]>([])
const childBoundingBoxes = ref<DOMRect[]>([])

function updateBoundingBoxes() {
  displayedChildren.value = props.modelValue
  childBoundingBoxes.value = itemNodes.value.map((node) => node.getBoundingClientRect())
}
watch(() => [props.modelValue, dragItem.value], updateBoundingBoxes)

const mouseHandler = bindings.handler({
  dragListItem(event) {
    if (!(event instanceof DragEvent)) return
    const target = event.target instanceof HTMLElement ? event.target : undefined
    if (target) {
      target.classList.add(DRAG_PREVIEW_CLASS)
      requestAnimationFrame(() => {
        target.classList.remove(DRAG_PREVIEW_CLASS)
      })
    }
    if (event.dataTransfer) {
      event.dataTransfer.effectAllowed = 'move'
      if (dragItem.value && props.toString) {
        event.dataTransfer.setData('text/plain', JSON.stringify(props.toString(dragItem.value)))
      }
      if (dragItem.value && props.toDragPayload) {
        event.dataTransfer.setData(
          mimeType.value,
          JSON.stringify(props.toDragPayload(dragItem.value)),
        )
      }
    }
  },
})

function handleDragAndReturnChildList(event: DragEvent) {
  const item = dragItem.value
  if (
    !event.dataTransfer ||
    !item ||
    !childBoundingBoxes.value.length ||
    (props.toDragPayload && !event.dataTransfer?.types.includes(mimeType.value))
  )
    return props.modelValue
  event.dataTransfer.dropEffect = 'move'
  event.preventDefault()
  const distances = childBoundingBoxes.value
    .map((box, i) => {
      const distance =
        Math.max(box.left - event.clientX, event.clientX - box.right) +
        Math.max(box.top - event.clientY, event.clientY - box.bottom)
      return { index: i, distance }
    })
    .sort(({ distance: a }, { distance: b }) => a - b)
  let insertIndex: number | undefined
  if (!distances[0] || distances[0].distance >= MAX_DISTANCE_PX) {
    // FIXME: Drag events currently do not fire outside the node; we need to listen to drag events on the workspace.
    // Ignored; the pointer is too far away so the element should be removed.
  } else if (!distances[1]) insertIndex = distances[0].index + 1
  else if (distances[1].distance > MAX_DISTANCE_PX)
    insertIndex = distances[0].index + (distances[1].index < distances[0].index ? 1 : 0)
  else if (Math.abs(distances[0].index - distances[1].index) === 1)
    insertIndex = Math.max(distances[0].index, distances[1].index)
  else insertIndex = Math.min(distances[0].index, distances[1].index) + 1
  const newChildren = [...props.modelValue]
  const itemIndex = newChildren.indexOf(item)
  newChildren.splice(newChildren.indexOf(item), 1)
  if (insertIndex != null)
    newChildren.splice(insertIndex - Number(itemIndex >= 0 && insertIndex > itemIndex), 0, item)
  return newChildren
}
</script>

<template>
  <div
    class="VectorWidget"
    @pointerdown="
      !$event.shiftKey && !$event.altKey && !$event.metaKey && $event.stopImmediatePropagation()
    "
  >
    <div
      class="vector-literal literal"
      @dragover="displayedChildren = handleDragAndReturnChildList($event)"
      @drop="
        emit('update:modelValue', handleDragAndReturnChildList($event)), (dragItem = undefined)
      "
    >
      <span class="token">[</span>
      <TransitionGroup tag="ul" name="list" class="items">
        <template v-for="(item, index) in modelValue" :key="props.getId?.(item) ?? index">
          <li v-if="index !== 0" class="token">,&nbsp;</li>
          <li
            ref="itemNodes"
            class="item"
            draggable="true"
            @dragstart="(dragItem = item), mouseHandler($event, false) || (dragItem = undefined)"
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

.list-move,
.list-enter-active,
.list-leave-active {
  transition: transform 0.5s ease;
}

.list-enter-from,
.list-leave-to {
  opacity: 0;
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
