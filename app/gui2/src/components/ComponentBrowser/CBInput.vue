<script setup lang="ts">
import { onMounted, ref, toRef, watch } from 'vue'

const props = withDefaults(defineProps<{ modelValue?: string; cursorPosition?: number }>(), {
  modelValue: '',
  cursorPosition: 0,
})
const emit = defineEmits<{
  (e: 'update:modelValue', input: string): void
  (e: 'update:cursorPosition', position: number | null): void
}>()

const inputField = ref<HTMLInputElement>()

onMounted(() => {
  if (inputField.value != null) {
    inputField.value.focus({ preventScroll: true })
  }
})


</script>

<template></template>

<style></style>
