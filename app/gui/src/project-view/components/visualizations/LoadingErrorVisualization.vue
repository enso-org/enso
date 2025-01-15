<script lang="ts">
export const name = 'Loading Error'
export const inputType = 'Any'
</script>

<script setup lang="ts">
const props = defineProps<{ data: { name: string; error: Error } }>()
</script>

<template>
  <div class="LoadingErrorVisualization">
    <div>Could not load visualization '<span v-text="props.data.name"></span>':</div>
    <div v-text="props.data.error.message"></div>
  </div>
</template>

<style scoped>
.LoadingErrorVisualization {
  height: 100%;
  padding: 0 1em;
}
</style>
