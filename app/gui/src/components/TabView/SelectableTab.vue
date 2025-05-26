<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { Icon } from '@/util/iconMetadata/iconName'
import { motion } from 'motion-v'
import { computed } from 'vue'
import CloseButton from '../CloseButton.vue'

const selected = defineModel<boolean>('selected')
const props = defineProps<{
  icon?: Icon | undefined
  label?: string | undefined
  tooltip?: string | undefined
  onClose?: (() => void) | undefined
}>()

const whenTooltip = computed(() => (props.label && !props.tooltip ? 'whenOverflow' : 'always'))
</script>

<template>
  <TooltipTrigger :when="whenTooltip">
    <template #default="triggerProps">
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
        <button role="tab" class="content" v-bind="triggerProps">
          <SvgIcon v-if="icon" :name="icon" />
          <slot />
          <span v-if="label" class="label">{{ label }}</span>
          <CloseButton v-if="onClose" @click="onClose" />
        </button>
      </div>
    </template>
    <template v-if="$slots.tooltip || tooltip || label" #tooltip>
      <slot name="tooltip">{{ tooltip ?? label }}</slot>
    </template>
  </TooltipTrigger>
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
