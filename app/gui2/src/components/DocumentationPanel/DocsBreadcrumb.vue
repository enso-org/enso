<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

const props = defineProps<{ text: string; icon?: Icon | undefined }>()
const emit = defineEmits<{ click: [] }>()
</script>

<template>
  <div class="Breadcrumb">
    <SvgIcon v-if="props.icon" :name="props.icon || ''" />
    <span @pointerdown="emit('click')" v-text="props.text"></span>
  </div>
</template>

<style scoped>
.Breadcrumb {
  user-select: none;
  cursor: pointer;
  display: flex;
  align-items: center;
  gap: 2px;
  overflow-x: hidden;
}

span {
  display: inline-block;
  height: 24px;
  padding: 1px 0px;
  color: white;
  font-weight: 500;
  font-style: normal;
  line-height: 20px;
  white-space: nowrap;
  overflow-x: hidden;
  text-overflow: ellipsis;
}

svg {
  color: white;
}
</style>
