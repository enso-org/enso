<script setup lang="ts">
const props = defineProps<{ text: string }>()
const emit = defineEmits<{ click: [] }>()
</script>

<template>
  <div class="NavBreadcrumb"><span @click="emit('click')" v-text="props.text"></span></div>
</template>

<style scoped>
span {
  display: inline-block;
  height: 20px;
  padding-top: 1px;
  padding-bottom: 1px;
}

.NavBreadcrumb {
  user-select: none;
  cursor: pointer;
  border-radius: var(--radius-full);

  > .blur-container {
    border-radius: var(--radius-full);
    background-color: var(--color-frame-bg);
    backdrop-filter: var(--backdrop-blur);
  }
}
</style>
