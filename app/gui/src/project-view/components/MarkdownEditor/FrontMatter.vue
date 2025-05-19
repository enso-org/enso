<script setup lang="ts">
const props = defineProps<{
  properties: object
}>()
</script>

<template>
  <div class="FrontMatter">
    {{ 'Start' }}
    {{ JSON.stringify(properties) }}
    {{ 'End' }}
  </div>
</template>

<style scoped>
.FrontMatter {
}
</style>
