<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

import Breadcrumb from '@/components/DocumentationPanel/DocsBreadcrumb.vue'

export interface Item {
  label: string
  icon?: string
}
const props = defineProps<{ breadcrumbs: Item[]; color: string, canGoForward: boolean, canGoBackward: boolean }>()
const emit = defineEmits<{ click: [index: number], backward: [], forward: [] }>()

/**
 * Shrink first and middle elements in the breacrumbs, keeping the original size of others.
 */
function shrinkFactor(index: number): number {
  const middle = Math.floor(props.breadcrumbs.length / 2)
  return (index === middle || index === 0) ? 100 : 0
}

const icon = (breadcrumb: Item, index: number) => {
  return index == props.breadcrumbs.length - 1 && breadcrumb.icon ? breadcrumb.icon : undefined
}
</script>

<template>
  <div class="Breadcrumbs" :style="{ 'background-color': color }">
    <div class="breadcrumbs-controls">
      <SvgIcon name="arrow_left" draggable="false" :class="['icon', 'button', 'arrow', { inactive: !props.canGoBackward }]" @pointerdown="emit('backward')" />
      <SvgIcon name="arrow_right" draggable="false" :class="['icon', 'button', 'arrow', { inactive: !props.canGoForward }]" @pointerdown="emit('forward')" />
    </div>
    <TransitionGroup name="breadcrumbs" appear>
    <template v-for="(breadcrumb, index) in props.breadcrumbs" :key="[index, breadcrumb.label]">
        <SvgIcon v-if="index > 0" name="arrow_right_head_only" class="arrow" />
        <Breadcrumb
          :text="breadcrumb.label"
          :icon="icon(breadcrumb, index)"
          :style="{ 'flex-shrink': shrinkFactor(index) }"
          @click="emit('click', index)"
        />
      </template>
    </TransitionGroup>
  </div>
</template>

<style scoped>
.Breadcrumbs {
  display: flex;
  height: 32px;
  padding: 8px 10px 8px 8px;
  align-items: center;
  gap: 2px;
  border-radius: 16px;
  transition: background-color 0.5s width 0.5s;
  max-width: 100%;
}

.breadcrumbs-controls {
  display: flex;
}

.inactive {
  opacity: 0.3;
}

.arrow {
  color: white;
}

.breadcrumbs-enter-active,
.breadcrumb-leave-active {
  transition: all 1s ease;
}


.breadcrumbs-enter-from {
  opacity: 0;
}
</style>
