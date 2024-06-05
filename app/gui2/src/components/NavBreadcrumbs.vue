<script setup lang="ts">
import NavBreadcrumb from '@/components/NavBreadcrumb.vue'
import SvgButton from '@/components/SvgButton.vue'

export interface BreadcrumbItem {
  label: string
  active: boolean
  editable: boolean
}

const props = defineProps<{ breadcrumbs: BreadcrumbItem[] }>()
const emit = defineEmits<{ selected: [index: number] }>()
</script>

<template>
  <div class="NavBreadcrumbs">
    <template v-for="(breadcrumb, index) in props.breadcrumbs" :key="index">
      <SvgButton
        v-if="index > 0"
        name="arrow_right_head_only"
        :disabled="breadcrumb.active"
        class="arrow"
      />
      <NavBreadcrumb
        :text="breadcrumb.label"
        :active="breadcrumb.active"
        :title="index === 0 ? 'Project Name' : ''"
        @click.stop="emit('selected', index)"
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
