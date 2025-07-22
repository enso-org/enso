<script setup lang="ts">
import type { Opt } from '@/util/data/opt'

const props = defineProps<{ target: Opt<HTMLElement> }>()
</script>

<template>
  <Teleport :to="props.target" :disabled="props.target == null">
    <slot />
  </Teleport>
</template>
