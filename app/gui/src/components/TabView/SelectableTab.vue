<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { Icon } from '@/util/iconMetadata/iconName'
import { motion } from 'motion-v'
import CloseButton from '../CloseButton.vue'

const selected = defineModel<boolean>('selected')
defineProps<{
  icon?: Icon | undefined
  label?: string | undefined
  onClose?: (() => void) | undefined
}>()
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
    <button role="tab" class="content">
      <SvgIcon v-if="icon" :name="icon" />
      <slot />
      <TooltipTrigger when="when-overflow">
        <span v-if="label" class="label">{{ label }}</span>
      </TooltipTrigger>
      <CloseButton v-if="onClose" @click="onClose" />
    </button>
  </div>
</template>

<style scoped>
.SelectableTab {
  position: relative;
  display: flex;
  flex-direction: row;
  height: 100%;
  padding: 8px;
  white-space: nowrap;
}

.underlying {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -1;
}

.content {
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

.label {
  max-width: 160px;
  overflow: hidden;
  text-overflow: ellipsis;
}
</style>
