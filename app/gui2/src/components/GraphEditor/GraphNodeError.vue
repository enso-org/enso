<script setup lang="ts">
const props = defineProps<{ error: string }>()
</script>

<template>
  <div class="GraphNodeError" v-text="props.error"></div>
</template>

<style scoped>
.GraphNodeError {
  display: flex;
  height: 24px;
  padding: 1px 8px;
  align-items: flex-start;
  gap: 10px;
  font-weight: 800;
  white-space: nowrap;
  border-radius: var(--radius-full);
  background-color: #e85252;
  color: var(--color-text-inversed);
  line-height: 20px;
}
</style>
