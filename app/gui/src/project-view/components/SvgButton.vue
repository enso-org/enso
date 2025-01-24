<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'

const _props = defineProps<{
  name?: Icon | URLString | undefined
  label?: string | undefined
  disabled?: boolean | undefined
  title?: string | undefined
}>()
</script>

<template>
  <MenuButton :disabled="disabled" class="SvgButton" :title="title">
    <SvgIcon v-if="name" :name="name" />
    <div v-if="label">{{ label }}</div>
  </MenuButton>
</template>

<style scoped>
.SvgButton {
  margin: -4px;
  gap: 4px;

  &.disabled {
    opacity: 0.3;
  }
}
</style>
