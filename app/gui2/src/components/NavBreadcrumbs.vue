<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

import NavBreadcrumb from '@/components/NavBreadcrumb.vue'

export interface BreadcrumbItem {
  label: string
  active: boolean
}

const props = defineProps<{ breadcrumbs: BreadcrumbItem[] }>()
const emit = defineEmits<{ selected: [index: number] }>()
</script>

<template>
  <div class="NavBreadcrumbs">
    <template v-for="(breadcrumb, index) in props.breadcrumbs" :key="index">
      <SvgIcon
        v-if="index > 0"
        name="arrow_right_head_only"
        :class="['arrow', { inactive: !breadcrumb.active }]"
      />
      <NavBreadcrumb
        :text="breadcrumb.label"
        :active="breadcrumb.active"
        @pointerdown="emit('selected', index)"
      />
    </template>
  </div>
</template>

<style scoped>
.NavBreadcrumbs {
  display: flex;
  align-items: center;
  gap: 2px;
}

.arrow {
  color: #666666;
}

.inactive {
  opacity: 0.4;
}
</style>
