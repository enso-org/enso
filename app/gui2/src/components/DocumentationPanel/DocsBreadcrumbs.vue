<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

import Breadcrumb from '@/components/DocumentationPanel/DocsBreadcrumb.vue'

export interface Item {
  label: string
}

const props = defineProps<{
  breadcrumbs: Item[]
  color: string
  icon: Icon
  canGoForward: boolean
  canGoBackward: boolean
}>()
const emit = defineEmits<{ click: [index: number]; backward: []; forward: [] }>()

/**
 * Shrink first and middle elements in the breacrumbs, keeping the original size of others.
 */
function shrinkFactor(index: number): number {
  const middle = Math.floor(props.breadcrumbs.length / 2)
  return index === middle || index === 0 ? 100 : 0
}
</script>

<template>
  <div class="Breadcrumbs" :style="{ 'background-color': color }">
    <div class="breadcrumbs-controls">
      <SvgIcon
        name="arrow_left"
        draggable="false"
        :class="['icon', 'button', 'arrow', { inactive: !props.canGoBackward }]"
        @pointerdown="emit('backward')"
      />
      <SvgIcon
        name="arrow_right"
        draggable="false"
        :class="['icon', 'button', 'arrow', { inactive: !props.canGoForward }]"
        @pointerdown="emit('forward')"
      />
    </div>
    <TransitionGroup name="breadcrumbs">
      <template v-for="(breadcrumb, index) in props.breadcrumbs" :key="[index, breadcrumb.label]">
        <SvgIcon v-if="index > 0" name="arrow_right_head_only" class="arrow" />
        <Breadcrumb
          :text="breadcrumb.label"
          :icon="index === props.breadcrumbs.length - 1 ? props.icon : undefined"
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
  transition: background-color 0.5s;
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
  transition: opacity 0.5s;
}

.breadcrumbs-move,
.breadcrumbs-enter-active,
.breadcrumb-leave-active {
  transition: all 0.3s ease;
}

.breadcrumbs-leave-active {
  display: none;
}

.breadcrumbs-enter-from {
  opacity: 0;
}
</style>
