<script setup lang="ts">
const props = defineProps<{
  data: unknown
}>()
</script>

<template>
  <span class="JsonErrorWidget" v-text="props.data" />
</template>

<style scoped>
.JsonErrorWidget {
  padding: 8px;
  padding-inline-start: 0;
}
</style>
