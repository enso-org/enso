<script lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { defineKeybinds } from '@/util/shortcuts'
import { ref } from 'vue'

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
  toJSON?: (item: T) => unknown
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: T[]] }>()

const dragItem = ref<T>()

const mouseHandler = bindings.handler({
  dragListItem(event) {
    if (!(event instanceof DragEvent)) return
    if (event.dataTransfer) {
      event.dataTransfer.effectAllowed = 'move'
      if (dragItem.value && props.toJSON) {
        event.dataTransfer.setData(
          props.dragMimeType ?? 'application/json',
          JSON.stringify(props.toJSON(dragItem.value)),
        )
      }
    }
    console.log('TODO: A list item is being dragged:', event.dataTransfer)
  },
})

function insertAt<T>(array: T[], index: number, item: T) {
  array.splice(index, 0, item)
  return array
}
</script>

<template>
  <div
    class="VectorWidget"
    @pointerdown="
      !$event.shiftKey && !$event.altKey && !$event.metaKey && $event.stopImmediatePropagation()
    "
  >
    <div class="vector-literal literal">
      <span class="token">[</span>
      <TransitionGroup tag="ul" class="items">
        <template v-for="(item, index) in modelValue" :key="props.getId?.(item) ?? index">
          <li
            v-if="index !== 0"
            class="token"
            @click="
              emit('update:modelValue', insertAt([...props.modelValue], index, props.default()))
            "
          >
            ,&nbsp;
          </li>
          <li
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

.move,
.enter-active,
.leave-active {
  transition: all 0.5s ease;
}

.enter-from,
.leave-to {
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
