<script setup lang="ts">
import { Opt } from '@/util/data/opt'

const props = defineProps<{ target: Opt<HTMLElement> }>()
</script>

<template>
  <Teleport v-if="props.target" :to="props.target">
    <slot />
  </Teleport>
  <slot v-else />
</template>
