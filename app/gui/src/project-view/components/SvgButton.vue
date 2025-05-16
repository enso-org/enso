<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'

defineProps<{
  name?: Icon | URLString | undefined
  label?: string | undefined
  disabled?: boolean | undefined
  title?: string | undefined
  extraClickZone?: number | undefined
}>()
const emit = defineEmits<{ activate: [] }>()
</script>

<template>
  <MenuButton
    :disabled="disabled"
    class="SvgButton"
    :title="title"
    :extraClickZone="extraClickZone"
    @activate="emit('activate')"
  >
    <SvgIcon v-if="name" :name="name" />
    <div v-if="label">{{ label }}</div>
  </MenuButton>
</template>

<style scoped>
.SvgButton {
  margin: -4px;
  gap: 4px;
  transition: opacity 0.2s;

  &.disabled {
    opacity: 0.3;
  }
}
</style>
