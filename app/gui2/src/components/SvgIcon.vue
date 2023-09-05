<script setup lang="ts">
import icons from '@/assets/icons.svg'

const props = defineProps<{ name: String }>()
</script>

<template>
  <svg>
    <use :href="`${icons}#${props.name}`"></use>
  </svg>
</template>

<style>
svg {
  width: 16px;
  height: 16px;
}
</style>
