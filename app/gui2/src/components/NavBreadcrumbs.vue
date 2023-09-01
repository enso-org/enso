<script setup lang="ts">
import ArrowRightHeadOnly from '@/assets/icons/arrow_right_head_only.svg'

import NavBreadcrumb from '@/components/NavBreadcrumb.vue'

const props = defineProps<{ breadcrumbs: string[] }>()
const emit = defineEmits<{ click: [index: number] }>()
</script>

<template>
  <div class="NavBreadcrumbs">
    <template v-for="(breadcrumb, index) in breadcrumbs">
      <img v-if="index > 0" :src="ArrowRightHeadOnly" />
      <NavBreadcrumb :text="breadcrumb" @click="emit('click', index)" />
    </template>
  </div>
</template>

<style scoped>
.NavBreadcrumbs {
  display: flex;
  gap: 2px;
}
</style>
