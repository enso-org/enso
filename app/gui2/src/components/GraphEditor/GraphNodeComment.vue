<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{ modelValue: string }>()

const paragraphs = computed(() => props.modelValue.split('\n\n'))
</script>

<template>
  <div class="GraphNodeComment">
    <p v-for="(paragraph, i) in paragraphs" :key="i" v-text="paragraph" />
  </div>
</template>

<style scoped>
.GraphNodeComment {
  padding: 1px 8px;
  font-weight: 800;
  white-space: nowrap;
  border-radius: var(--radius-full);
  color: var(--color-text);
  line-height: 20px;
  background-color: white;
}
</style>
