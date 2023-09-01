<script setup lang="ts">
export type Operator = '+' | '-' | '*' | '/'

const props = defineProps<{
  left: string
  operator: Operator
  right: string
}>()
const emit = defineEmits<{}>()
</script>

<template>
  <div class="BinaryOperator">
    <div class="operator" ></div>
  </div>
</template>

<style scoped>
.BinaryOperator {}
</style>
