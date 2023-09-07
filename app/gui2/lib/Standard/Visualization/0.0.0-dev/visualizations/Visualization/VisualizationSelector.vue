<script setup lang="ts">
const props = defineProps<{ types: string[] }>()
const emit = defineEmits<{ 'update:type': [type: string] }>()
</script>

<template>
  <div class="VisualizationSelector">
    <div class="background"></div>
    <ul>
      <li
        v-for="type_ in types"
        :key="type_"
        v-text="type_"
        @click="emit('update:type', type_)"
      ></li>
    </ul>
  </div>
</template>

<style scoped>
.VisualizationSelector {
  user-select: none;
  position: absolute;
  border-radius: 16px;
  top: 100%;
  margin-top: 12px;
  left: -12px;

  > * {
    position: relative;
  }

  > .background {
    position: absolute;
    width: 100%;
    height: 100%;
    border-radius: 16px;
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
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
