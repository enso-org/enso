<script setup lang="ts">
const props = withDefaults(defineProps<{ modelValue?: string; cursorPosition?: number }>(), {
  modelValue: '',
  cursorPosition: 0,
})
const emit = defineEmits<{
  (e: 'update:modelValue', input: string): void
  (e: 'update:cursorPosition', position: number): void
}>()
</script>

<template>
  <div class="CBInput"><input ref="inputField" v-model="modelValue" /></div>
</template>
