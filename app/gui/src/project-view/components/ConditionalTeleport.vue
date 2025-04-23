<script setup lang="ts">
const props = defineProps<{ target: HTMLElement | undefined }>()
</script>

<template>
  <Teleport v-if="props.target" :to="props.target" v-bind="$attrs">
    <slot />
  </Teleport>
  <slot v-else v-bind="$attrs" />
</template>
