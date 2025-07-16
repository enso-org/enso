<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { Icon } from '@/util/iconMetadata/iconName'
import { AnimatePresence, motion } from 'motion-v'
import { computed } from 'vue'
import CloseButton from '../CloseButton.vue'

const {
  selected,
  selectionLayoutId,
  label,
  tooltip,
  orientation = 'horizontal',
  enabled = true,
} = defineProps<{
  selected: boolean
  selectionLayoutId: string
  icon?: Icon | undefined
  label?: string | undefined
  tooltip?: string | undefined
  orientation?: 'horizontal' | 'vertical'
  enabled?: boolean
  onClose?: (() => void) | undefined
}>()
// We don't use defineModel, because we want to let component user decide what to do on selection
// change.
const emit = defineEmits<{ 'update:selected': [value: boolean] }>()

const tooltipPlacement = computed(() => (orientation === 'horizontal' ? 'top' : 'left'))
const whenTooltip = computed(() => (label && !tooltip ? 'whenOverflow' : 'always'))

const VARIANTS = {
  hidden: { opacity: 0 },
  visible: { opacity: 1 },
}
</script>

<template>
  <TooltipTrigger :placement="tooltipPlacement" :when="whenTooltip">
    <template #default="triggerProps">
      <div class="SelectableTab" :class="orientation" @click="emit('update:selected', !selected)">
        <AnimatePresence :initial="selected != null">
          <motion.div
            v-if="selected"
            class="underlying"
            :class="orientation"
            :layoutId="selectionLayoutId"
            :variants="VARIANTS"
            initial="hidden"
            animate="visible"
            exit="hidden"
          />
        </AnimatePresence>
        <button
          role="tab"
          :aria-label="tooltip ?? label ?? ''"
          class="tabContent"
          :class="{ enabled }"
          :disabled="!enabled"
          v-bind="triggerProps"
        >
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
  --selection-color: var(--color-dashboard-background);
  --border-radius: 2rem;
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  padding: 8px;
  white-space: nowrap;

  &.horizontal {
    height: 100%;
  }

  &.vertical {
    width: 100%;
  }
}

.underlying {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -1;
  background-color: var(--selection-color);

  &:before,
  &:after {
    content: '';
    position: absolute;
    width: 1.25rem;
    height: 1.25rem;
    background-image: radial-gradient(circle at 0 0, transparent 70%, var(--selection-color) 70%);
  }

  &.horizontal {
    border-top-left-radius: var(--border-radius);
    border-top-right-radius: var(--border-radius);

    &:before {
      left: -1.25rem;
      bottom: 0;
    }

    &:after {
      transform: rotate(90deg);
      right: -1.25rem;
      bottom: 0;
    }
  }

  &.vertical {
    border-top-right-radius: var(--border-radius);
    border-bottom-right-radius: var(--border-radius);

    &:before {
      transform: rotate(90deg);
      top: -1.25rem;
    }

    &:after {
      transform: rotate(180deg);
      bottom: -1.25rem;
    }
  }
}

.tabContent {
  height: 100%;
  padding: 8px;
  display: flex;
  border-radius: var(--radius-full);
  flex-direction: row;
  align-items: center;
  gap: 12px;
  transition: background-color 0.3s;
  cursor: not-allowed;
  opacity: 0.2;

  &.enabled {
    opacity: 1;
    cursor: pointer;

    &:hover,
    &:active {
      background-color: var(--tab-highlight);
    }
  }
}

.label {
  max-width: 160px;
  overflow: hidden;
  text-overflow: ellipsis;
}
</style>
