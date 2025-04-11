<script setup lang="ts">
import { motion } from 'motion-v'

const selected = defineModel<boolean>('selected')
</script>

<template>
  <div class="SelectableTab" @click="selected = true">
    <motion.div v-if="selected" class="underlying" layoutId="tab-highlight">
      <!-- TODO[ao]: Style copied from dashboard. Anyone is welcome to port it <style scoped> 
        in their free time -->
      <div class="h-full w-full rounded-t-4xl bg-dashboard" />
      <div
        class="absolute -left-5 bottom-0 aspect-square w-5 -rotate-90 [background:radial-gradient(circle_at_100%_0%,_transparent_70%,_var(--color-dashboard-background)_70%)]"
      />
      <div
        class="absolute -right-5 bottom-0 aspect-square w-5 -rotate-90 [background:radial-gradient(circle_at_100%_100%,_transparent_70%,_var(--color-dashboard-background)_70%)]"
      />
    </motion.div>
    <button role="tab" class="label">
      <slot />
    </button>
  </div>
</template>

<style scoped>
.SelectableTab {
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  height: 100%;
  z-index: 0;
  padding: 8px;
}

.underlying {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -1;
}

.label {
  height: 100%;
  padding: 8px;
  display: flex;
  border-radius: var(--radius-full);
  flex-direction: row;
  align-items: center;
  gap: 12px;
  transition: background-color 0.3s;

  &:hover,
  &:focus,
  &:active {
    background-color: var(--color-dashboard-background);
  }
}
</style>
