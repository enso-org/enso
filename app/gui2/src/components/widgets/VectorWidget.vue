<script setup lang="ts" generic="T">
import { nodeEditBindings } from '@/bindings'
import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const props = defineProps<{
  modelValue: T[]
  default: () => T
  getId?: (item: T) => PropertyKey | undefined
  dragMimeType?: string
  toJSON?: (item: T) => unknown
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: T[]] }>()

const dragItem = ref<T>()

const nodeEditHandler = nodeEditBindings.handler({
  dragListItem(event) {
    if (!(event instanceof DragEvent)) return
    console.log('TODO: A list item is being dragged.')
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = 'move'
      if (dragItem.value && props.toJSON) {
        event.dataTransfer.setData(
          props.dragMimeType ?? 'application/json',
          JSON.stringify(props.toJSON(dragItem.value)),
        )
      }
    }
  },
})
</script>

<template>
  <div class="VectorWidget" @pointerdown.stop>
    <TransitionGroup tag="ul" class="items">
      <li
        v-for="(item, index) in modelValue"
        :key="props.getId?.(item) ?? index"
        draggable="true"
        class="item"
        @dragstart="console.log($event), nodeEditHandler($event) && (dragItem = item)"
      >
        <slot :item="item"></slot>
      </li>
    </TransitionGroup>
    <SvgIcon
      class="add-item"
      name="vector_add"
      @pointerdown.stop
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

.items {
  display: flex;
}

.item::before,
.item::after {
  color: rgb(255 255 255 / 0.33);
  vertical-align: middle;
  align-items: center;
  display: inline-flex;
  height: 24px;
}

.item:first-child::before {
  content: '[';
}

.item::after {
  content: ',\00a0';
}

.item:last-child::after {
  content: ']';
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
