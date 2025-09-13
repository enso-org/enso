<script setup lang="ts">
defineProps<{ when: boolean }>()
</script>

<template>
  <KeepAlive include="KeepAlive">
    <KeepAlive v-if="when"><slot /></KeepAlive>
    <template v-else><slot /></template>
  </KeepAlive>
</template>
