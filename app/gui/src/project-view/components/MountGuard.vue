<script setup lang="ts">
/** @file Workaround that reduces the impact of https://github.com/enso-org/enso/issues/13667. */

import { onBeforeUnmount, onMounted, ref } from 'vue'

const mounted = ref(false)

onMounted(() => (mounted.value = true))
onBeforeUnmount(() => {
  if (!mounted.value)
    console.warn('Component unmounted without mount hook having been run (bug #13667)')
  mounted.value = false
})
</script>

<template>
  <slot v-if="mounted" />
</template>
