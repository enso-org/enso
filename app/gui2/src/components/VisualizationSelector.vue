<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'

const props = defineProps<{ types: string[] }>()
const emit = defineEmits<{ hide: []; 'update:type': [type: string] }>()

const rootNode = ref<HTMLElement>()

function onClick(event: MouseEvent) {
  if (event.target instanceof Node && rootNode.value?.contains(event.target)) {
    return
  }
  emit('hide')
}

onMounted(() => {
  document.addEventListener('click', onClick)
})

onUnmounted(() => {
  document.removeEventListener('click', onClick)
})
</script>

<template>
  <div ref="rootNode" class="VisualizationSelector">
    <div class="background"></div>
    <ul>
      <li
        v-for="type_ in types"
        :key="type_"
        @click="emit('update:type', type_)"
        v-text="type_"
      ></li>
    </ul>
  </div>
</template>

<style scoped>
.VisualizationSelector {
  /* Required for it to show above Mapbox's information button. */
  z-index: 2;
  user-select: none;
  position: absolute;
  border-radius: 16px;
  top: 100%;
  margin-top: 12px;
  left: -12px;
}

.VisualizationSelector > * {
  position: relative;
}

.VisualizationSelector > .background {
  position: absolute;
  width: 100%;
  height: 100%;
  border-radius: 16px;
  background: var(--color-app-bg);
  backdrop-filter: var(--blur-app-bg);
}

ul {
  display: flex;
  flex-flow: column;
  list-style-type: none;
  padding: 4px;
}

li {
  cursor: pointer;
  padding: 0 8px;
  border-radius: 12px;
  white-space: nowrap;

  &:hover {
    background: var(--color-menu-entry-hover-bg);
  }
}
</style>
