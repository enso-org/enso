<script setup lang="ts">
import { motion } from 'motion-v'
</script>

<template>
  <motion.div class="CommandPalette" :animate="{ y: 100 }"></motion.div>
</template>

<style scoped>
:root {
  border-radius: var(--rounded-default);
}
</style>
